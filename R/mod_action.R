action_ui_checklist <- function(id) {
  ns <- NS(id)

  checkboxGroupInput(
    ns("check"),
    NULL,
    choices = the$checklist[[id]],
    width = "100%"
  )
}

action_ui_button <- function(id, face) {
  ns <- NS(id)
  actionButton(ns("btn"), face, width = "100%")
}

action_ui_rdsfile <- function(id, step) {
  ns <- NS(id)
  tagList(
    p(paste0("Step ", step, " の自動生成ファイル")),
    choose_file_ui(
      ns("rds"), "選択",
      paste0("Step ", step, " の結果データ (step", step, ".rds)")
    )
  )
}


action_server <- function(id, fun, rv) {
  ns <- NS(id)
  checklist <- the$checklist[[id]]

  moduleServer(id, function(input, output, session) {

    observe({
      updateActionButton(session,
        "btn",
        disabled = !(n_checked(input$check) == length(checklist))
      )
    }) |>
      bindEvent(input$check, ignoreNULL = FALSE)

    observe({
      validate(
        need(
          n_checked(input$check) == length(checklist),
          "All items must be checked."
        ),
      )
      rds <- fun(rv, input, output, session)

      if (is.null(rds)) {
        showNotification("失敗しました。", type = "error", duration = 3)
      } else {
        updateTextInput(inputId = "rds-txt", value = rds)
        notification <-
          showNotification("完了しました。", type = "message", duration = 3)
      }
    }) |>
      bindEvent(input$btn)
  })
}
