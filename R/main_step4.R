step4_ui <- function() {
  ns <- NS("step4")

  nav_panel("4. 確定", value = "step4", page_fixed(
    div_(
      accordion(
        accordion_panel(
          "必要ファイル",
          action_ui_rdsfile("step3", step = 3),
          p("進級判定資料（データ）"),
          choose_file_ui("gakuen-xlsx", "選択", "成績データ（GAKUEN）",
                         value = path_exists(the$cfg[["gakuen-xlsx"]])),
          checkboxInput(ns("mikomi"), "見込判定で動作確認する", FALSE)
        ),
        accordion_panel(
          "チェックリスト",
          action_ui_checklist("step4")
        )
      )
    ),
    div_(
      action_ui_button("step4", "成績判定を反映する")
    ),
    div_(
      layout_columns(
        card(
          card_header("配属一覧", class = "bg-light"),
          DT::dataTableOutput(ns("match-result")) |> shinycssloaders::withSpinner()
        ),
        card(
          card_header("空きゼミ一覧", class = "bg-light"),
          DT::dataTableOutput(ns("open-slots")) |> shinycssloaders::withSpinner()
        ),
        col_widths = c(6, 6), height = "300px"
      )
    )
  ))
}


step4_server <- function(id = "step4") {
  moduleServer(id, function(input, output, session) {
    observe({
      rdsfile <- file.path(rv[["results-dir"]], "step4.rds")
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


step4_action <- function(rv, input, output, session) {
  cols <- the$cols
  rv <- isolate(rv)

  hantei <- if (isolate(input$mikomi)) {
    cols$gakuen$label0
  } else {
    cols$gakuen$label
  }

  students_xlsx <- tryRead(rv[["students-list"]],
                           "学生一覧のエクセルファイルが指定されていません。",
                           col_types = the$col_types$admin_st)
  faculty_xlsx <- tryRead(rv[["faculty-list"]],
                          "ゼミ一覧のエクセルファイルが指定されていません。",
                          col_types = the$col_types$admin_fc)
  rds2 <- tryRead(file.path(rv[["results-dir"]], "step2.rds"),
                  "step2.rds が指定されていません。")
  rds3 <- tryRead(file.path(rv[["results-dir"]], "step3.rds"),
                  "step3.rds が指定されていません。")
  gakuen_xls <- tryRead(rv[["gakuen-xlsx"]],
                        "GAKUENのエクセルファイルが指定されていません。")

  if (!hantei %in% colnames(gakuen_xls)) {
    shinyalert::shinyalert("Error!",
                           paste("列", hantei, "が存在しません。"))
    return(NULL)
  }

  mitasu_id <-
    gakuen_xls |>
    dplyr::group_by(.data[[cols$gakuen$stid]]) |>
    dplyr::select(!!cols$gakuen$stid, !!hantei) |>
    dplyr::distinct() |>
    dplyr::ungroup() |>
    dplyr::filter(.data[[hantei]] == cols$gakuen$code) |>
    dplyr::pull(!!cols$gakuen$stid)

  df <- list()

  df$main <-
    rds3$main |>
    dplyr::mutate(
      !!cols$main$stname := dplyr::if_else(
        .data[[cols$match$student]] %in% mitasu_id,
        paste0("*", .data[[cols$main$stname]]),
        .data[[cols$main$stname]]),

      !!cols$main$stid_name := dplyr::if_else(
        .data[[cols$match$student]] %in% mitasu_id,
        paste0("*", .data[[cols$main$stid_name]]),
        .data[[cols$main$stid_name]])
    ) |>
    dplyr::mutate(
      !!cols$match$student := dplyr::if_else(
        .data[[cols$match$student]] %in% mitasu_id,
        paste0("*", .data[[cols$match$student]]),
        .data[[cols$match$student]])
    )

  df$display <-
    df$main |>
    dplyr::select(
      !!cols$moodle$stid := !!cols$match$student,
      !!cols$moodle$seminar := !!cols$main$fcname
    )


  df$notfull <-
    df$main |>
    dplyr::filter(!startsWith(.data[[cols$match$student]], "*")) |>
    dplyr::group_by(.data[[cols$main$fcname]], .data[[cols$match$seminar]]) |>
    dplyr::summarize(!!cols$notfull$openslots := rv[["slots"]] - dplyr::n(),
                     .groups = "drop"
    ) |>
    dplyr::select(!!cols$notfull$seminar := !!cols$main$fcname,
                  !!cols$notfull$openslots) |>
    dplyr::filter(.data[[cols$notfull$openslots]] > 0)


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
  rds_file <- file.path(rv[["results-dir"]], "step4.rds")
  saveRDS(df, rds_file)

  ### 空きゼミ一覧
  readr::write_excel_csv(
    df$notfull,
    file = file.path(rv[["results-dir"]], get_report(4, "notfull"))
  )

  ## 文書作成
  slots <- max(rv[["slots"]], df$main[[cols$main$num]])
  wide <- wide_table(df, faculty_xlsx, slots = slots)


  ## 掲示確認用
  readr::write_excel_csv(
    df$display,
    file = file.path(rv[["results-dir"]], get_report(4, "display"))
  )

  ## 配属状況確認（名前入り）
  readr::write_excel_csv(
    wide$name,
    file = file.path(rv[["results-dir"]], get_report(4, "widename"))
  )


  ## 配属状況確認（学籍番号のみ）
  readr::write_excel_csv(
    wide$id,
    file = file.path(rv[["results-dir"]], get_report(4, "wideid"))
  )


  ## Moodle
  moodle_page(df,
              outdir = rv[["results-dir"]],
              step = 4,
              template_file = rv[["step4-template"]]
  )

  ## 履修登録
  registration <-
    df$main |>
    dplyr::filter(!startsWith(.data[[cols$match$student]], "*")) |>
    dplyr::select(!!cols$registration$stid := !!cols$match$student,
                  !!cols$registration$classid)

  readr::write_excel_csv(
    registration,
    file = file.path(rv[["results-dir"]], get_report(4, "registration"))
  )



  rds_file

}
