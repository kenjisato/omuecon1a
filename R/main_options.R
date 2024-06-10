option_ui <- function() {
  nav_panel(
    icon("gear"),
    id = "options-panel",
    page_fixed(
      div_(
        fill = FALSE,
        h5("Moodle Templates"),
        p("初回用"),
        choose_file_ui("step2-template", "選択",
          msg = "【初回】Moodle公開用テンプレート",
          value = "step2_template.Rmd"
        ),
        p("更新版"),
        choose_file_ui("step3-template", "選択",
          msg = "【更新】Moodle公開用テンプレート",
          value = "step3_template.Rmd"
        ),
        p("確定版"),
        choose_file_ui("step4-template", "選択",
          msg = "【確定】Moodle公開用テンプレート",
          value = "step4_template.Rmd"
        )
      ),
      div_(
        fill = FALSE,
        h5("Advanced"),
        textInput("random-seed", label = "乱数シード", value = 12345, width = "100%"),
        textInput("digits", label = "学番桁数", value = 8, width = "100%")
      ),
      div_(
        fill = FALSE,
        h5("Danger"),
        actionButton("reset-indexeddb",
          label = "Reset IndexedDB",
          width = "90%", class = "btn-danger"
        )
      )
    )
  )
}

options_server <- function(rv, input, output, session) {
  observe({
    shinyalert::shinyalert(
      title = "Caution!",
      text = "ブラウザに保存された設定情報が削除されます。ファイルは削除されません。",
      size = "s",
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "No",
      callbackR = function(x) {
        if (x) {
          shinyStorePlus::clearStore(appId = appid(), session)
        }
      }
    )
  }) |>
    bindEvent(input[["reset-indexeddb"]])

  observe(rv[["slots"]] <- as.integer(input$slots))
  observe(rv[["digits"]] <- as.integer(input$digits))
  observe(rv[["random-seed"]] <- as.integer(input[["random-seed"]]))

  # Options
  choose_file_server("step2-template", rv, "root-dir", filetype = "Rmd")
  choose_file_server("step3-template", rv, "root-dir", filetype = "Rmd")
  choose_file_server("step4-template", rv, "root-dir", filetype = "Rmd")
}
