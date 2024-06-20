step3_ui <- function() {
  ns <- NS("step3")

  nav_panel(
    "3. 修正",
    value = "step3",
    page_fixed(
      div_(
        accordion(
          accordion_panel(
            "必要ファイル",
            action_ui_rdsfile("step2", step = 2),
            p("配属変更 希望者リスト"),
            choose_file_ui("change-xlsx", "選択",
                           "配属変更表を選択してください。",
                           value = path_exists(the$cfg[["change-xlsx"]]))
          ),
          accordion_panel(
            "チェックリスト",
            action_ui_checklist("step3")
          )
        )
      ),
      div_(
        action_ui_button("step3", "配属修正を実行する")
      ),
      div_(
        layout_columns(
          card(
            card_header("配属一覧", class = "bg-light"),
            DT::dataTableOutput(ns("match-result")) |> shinycssloaders::withSpinner(),
            full_screen = TRUE
          ),
          card(
            card_header("空きゼミ一覧", class = "bg-light"),
            DT::dataTableOutput(ns("open-slots")) |> shinycssloaders::withSpinner(),
            full_screen = TRUE
          ),
          col_widths = c(6, 6), height = "300px"
        )
      )
    ),
  )
}


step3_server <- function(id = "step3") {
  moduleServer(id, function(input, output, session) {
    observe({
      rdsfile <- file.path(rv[["results-dir"]], "step3.rds")
      if (file.exists(rdsfile)) {
        rds <- readRDS(rdsfile)

        output[["match-result"]] <- DT::renderDataTable({
          DT::datatable(rds$display,
                        rownames = FALSE, selection = "single",
                        options = list(pageLength = 10, paging = FALSE,
                                       searching = TRUE)
          )
        })

        output[["open-slots"]] <- DT::renderDataTable({
          DT::datatable(rds$notfull,
                        options = list(paging = FALSE, searching = TRUE),
                        rownames = FALSE, selection = "single"
          )
        })
      }
    }) |>
      bindEvent(
        rv[["results-dir"]]
      )
  })
}



# step3 の本体
step3_action <- function(rv, input, output, session) {
  cols <- the$cols

  rv <- isolate(rv)
  students_xlsx <- tryRead(rv[["students-list"]],
                           "学生一覧のエクセルファイルが指定されていません。",
                           col_types = the$col_types$admin_st)
  faculty_xlsx <- tryRead(rv[["faculty-list"]],
                          "ゼミ一覧のエクセルファイルが指定されていません。",
                          col_types = the$col_types$admin_fc)

  rds <- tryRead(file.path(rv[["results-dir"]], "step2.rds"),
                  "step2.rds が指定されていません。")

  change_xls <- tryRead(rv[["change-xlsx"]],
                        "変更希望一覧のエクセルファイルが指定されていません。",
                        col_types = the$col_types$change)

  ## Updated match_table
  matching_chg0 <-
    rds$result$match_table |>
    dplyr::rows_upsert(
      change_xls |>
        dplyr::select(!!cols$change$student, !!cols$change$seminar),
      by = cols$match$student
    )

  ## 学生氏名を紐づける
  matching_df_chg0 <-
    matching_chg0 |>
    dplyr::left_join(
      students_xlsx,
      by = dplyr::join_by(!!cols$match$student == !!cols$admin$stid)
    )

  ## 事務管理データに存在しない学生氏名を補正する
  matching_df_chg1 <-
    matching_df_chg0 |>
    dplyr::rows_patch(
      change_xls |>
        dplyr::select(
          !!cols$change$student,
          !!cols$admin$stname := !!cols$change$name
        ),
      by = cols$match$student
    )

  .stid <- rlang::sym(cols$match$student)
  .stname <- rlang::sym(cols$admin$stname)
  .seminar <- rlang::sym(cols$match$seminar)
  .fcname <- rlang::sym(cols$main$fcname)
  .openslots <- rlang::sym(cols$notfull$openslots)

  df <- list()

  df$main <-
    matching_df_chg1 |>
    dplyr::mutate(
      !!cols$main$stid_name := paste(last_n(!!.stid, 3), !!.stname)
    ) |>
    dplyr::left_join(
      faculty_xlsx,
      by = dplyr::join_by(!!cols$match$seminar == !!cols$admin$fcid)
    ) |>
    dplyr::rename(
      !!cols$main$stname := !!cols$admin$stname,
      !!cols$main$fcname := !!cols$admin$fcname
    ) |>
    dplyr::group_by(!!.seminar) |>
    dplyr::mutate(!!cols$main$num := dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(-!!cols$admin$gpa)


  ## 掲示用の配属ゼミ一覧
  df$display <-
    df$main |>
    dplyr::select(
      !!cols$moodle$stid := !!cols$match$student,
      !!cols$moodle$seminar := !!cols$main$fcname
    )

  ## 修正後の空きゼミ

  df$notfull <-
    df$main |>
    dplyr::group_by(!!.fcname, !!.seminar) |>
    dplyr::summarize(!!cols$notfull$openslots := rv[["slots"]] - dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::select(!!cols$notfull$seminar := !!.fcname, !!.openslots) |>
    dplyr::filter(!!.openslots > 0)


  ## 結果を画面表示
  output[["open-slots"]] <- DT::renderDataTable({
    DT::datatable(df$notfull,
      options = list(paging = FALSE, searching = TRUE),
      rownames = FALSE, selection = "single"
    )
  })

  output[["match-result"]] <- DT::renderDataTable({
    DT::datatable(df$display,
      rownames = FALSE, selection = "single",
      options = list(pageLength = 10, paging = FALSE, searching = TRUE)
    )
  })

  ## 修正後データの保存
  rds_file <- file.path(rv[["results-dir"]], "step3.rds")
  saveRDS(df, rds_file)


  ### 空きゼミ一覧
  readr::write_excel_csv(
    df$notfull,
    file = file.path(rv[["results-dir"]], get_report(3, "notfull"))
  )

  ## 文書作成
  slots <- max(rv[["slots"]], df$main[[cols$main$num]])
  wide <- wide_table(df, faculty_xlsx, slots = slots)


  ## 掲示確認用
  readr::write_excel_csv(
    df$display,
    file = file.path(rv[["results-dir"]], get_report(3, "display"))
  )

  ## 配属状況確認（名前入り）
  readr::write_excel_csv(
    wide$name,
    file = file.path(rv[["results-dir"]], get_report(3, "widename"))
  )


  ## 配属状況確認（学籍番号のみ）
  readr::write_excel_csv(
    wide$id,
    file = file.path(rv[["results-dir"]], get_report(3, "wideid"))
  )


  ## Moodle
  moodle_page(df,
    outdir = rv[["results-dir"]],
    step = 3,
    template_file = rv[["step3-template"]]
  )

  rds_file
}
