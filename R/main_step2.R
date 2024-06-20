step2_ui <- function() {
  ns <- NS("step2")

  nav_panel(
    "2. マッチング",
    value = "step2",
    page_fixed(
      div_(
        accordion(
          accordion_panel("チェックリスト",
                          action_ui_checklist("step2")
          )
        )
      ),
      div_(
        action_ui_button("step2", "マッチングを実行する")
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
    )
  )
}

step2_server <- function(id = "step2") {
  moduleServer(id, function(input, output, session) {
    observe({
      rdsfile <- file.path(rv[["results-dir"]], "step2.rds")
      if (file.exists(rdsfile)) {
        rds <- readRDS(rdsfile)

        output[["open-slots"]] <- DT::renderDataTable({
          DT::datatable(rds$disp$notfull,
            options = list(paging = FALSE, searching = TRUE),
            rownames = FALSE, selection = "single"
          )
        })

        output[["match-result"]] <- DT::renderDataTable({
          DT::datatable(rds$disp$display,
            rownames = FALSE, selection = "single",
            options = list(pageLength = 10, paging = FALSE, searching = TRUE)
          )
        })
      }
    }) |>
      bindEvent(
        rv[["results-dir"]]
      )
  })
}


# matching-run の本体
step2_action <- function(rv, input, output, session) {
  rv <- isolate(rv)

  students_xlsx <- tryRead(rv[["students-list"]],
                           "学生一覧のエクセルファイルが指定されていません。",
                           col_types = the$col_types$admin_st)
  faculty_xlsx <- tryRead(rv[["faculty-list"]],
                          "ゼミ一覧のエクセルファイルが指定されていません。",
                          cl_types = the$col_types$admin_fc)

  util <- matching_utils(
    student_list = students_xlsx,
    faculty_list = faculty_xlsx,
    dir_student = rv[["students-dir"]],
    dir_faculty = rv[["faculty-dir"]],
    nc = rv[["digits"]],
    seed = rv[["random-seed"]]
  )

  # DAアルゴリズムによるマッチング
  result <- matching_compute(util, slots = rv[["slots"]])
  df <- match_table(util, result, students_xlsx, faculty_xlsx)

  # 結果の保存
  matching_result <- list(
    util = util,
    result = result,
    disp = df,
    seed = rv[["random-seed"]]
  )

  rds_file <- file.path(rv[["results-dir"]], "step2.rds")
  saveRDS(matching_result, rds_file)

  ## 文書作成

  wide <- wide_table(df, faculty_xlsx, rv[["slots"]])

  readr::write_excel_csv(
    df$notfull,
    file = file.path(rv[["results-dir"]], get_report(2, "notfull"))
  )

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

  ## =============================================================================
  ## 【step2_マッチ掲示用.csv】
  ## =============================================================================
  ##
  ## > 学籍番号 配属ゼミ
  ## > <chr>    <chr>
  ## > 1 STIDst1  教員A
  ## > 2 STIDst2  教員A
  ## > 3 STIDst3  教員C
  ## > 4 STIDst4  教員B
  ## > 5 STIDst5  教員B
  ## > 6 STIDst6  教員C
  ## >
  readr::write_excel_csv(
    df$display,
    file = file.path(rv[["results-dir"]], get_report(2, "display"))
  )


  ## ========================================
  ## 【step2_マッチ名前入り.csv】
  ## ========================================
  ##
  ## 通番 教員A       教員B       教員C
  ## <int> <chr>       <chr>       <chr>
  ##   1 "st2 学生2" "st1 学生1" "st6 学生6"
  ##   2 "st3 学生3" "st5 学生5" ""
  ##   3 "st4 学生4" ""          ""
  ##   4 ""          ""          ""
  ##
  readr::write_excel_csv(
    wide$name,
    file = file.path(rv[["results-dir"]], get_report(2, "widename"))
  )


  ## ========================================
  ## 【step2_マッチ学籍番号.csv】
  ## ========================================
  ##
  ##  通番 教員A     教員B     教員C
  ## <int> <chr>     <chr>     <chr>
  ##   1 "STIDst2" "STIDst1" "STIDst6"
  ##   2 "STIDst3" "STIDst5" ""
  ##   3 "STIDst4" ""        ""
  ##   4 ""        ""        ""
  ##
  readr::write_excel_csv(
    wide$id,
    file = file.path(rv[["results-dir"]], get_report(2, "wideid"))
  )


  ## ============================================================================
  ## 【step2_マッチMoodle用.html】
  ## ============================================================================
  moodle_page(df,
    outdir = rv[["results-dir"]],
    step = 2,
    template_file = rv[["step2-template"]]
  )


  ## =============================================================================
  ## 【目検チェック用】step2_チェック_学生側評価.csv, step2_チェック_教員側評価.csv
  ## =============================================================================
  eyeballing(util)

  ## =============================================================================
  ## 【統計情報】
  ## =============================================================================
  statistics(util, result)

  rds_file
}
