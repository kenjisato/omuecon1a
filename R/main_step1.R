## 1. データ収集 ----

step1_ui <- function() {
  ns <- NS("step1")

  nav_panel("1. データ", value = "step1", page_fixed(
    div_(
      h5("事務管理データ"),
      p("ゼミ一覧"),
      choose_file_ui("faculty-list", "選択", "ゼミ一覧のファイルを選んでください。",
                     value = path_exists(the$cfg[["faculty-list"]])),
      p("学生一覧"),
      choose_file_ui("students-list", "選択", "対象学生一覧のファイルを選んでください。",
                     value = path_exists(the$cfg[["students-list"]]))
    ),
    div_(
      h5("教員提出データ"),
      list_files_ui(
        "faculty-xlsx",
        buttons = actionButton(ns("faculty-verify"), "検証",
                               class = "btn-info", width = "100%")
      ),
      class = "submit-data"
    ),
    div_(
      h5("学生提出データ"),
      list_files_ui(
        "students-xlsx",
        buttons =
          tagList(
            actionButton(ns("rename-btn"), "リネーム",
                         class = "btn-info mb-1", width = "100%"),
            actionButton(ns("students-verify"), "検証",
                         class = "btn-info", width = "100%"))
      ),
      class = "submit-data"
    )
  ))
}

step1_server <- function(id = "step1",  files) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(rv[["students-dir"]])
      n <- rename_student_excel(rv[["students-dir"]], rv[["digits"]])

      showModal(
        modalDialog(
          paste("Files renamed: ", n),
          size = "l",
          title = "Message"
        )
      )

      files(list.files(rv[["students-dir"]], pattern = "\\.xlsx$"))
    }) |>
      bindEvent(
        input[["rename-btn"]]
      )

    observe({
      req(rv[["faculty-dir"]], rv[["students-list"]])

      students <- readxl::read_xlsx(rv[["students-list"]])
      files <- list.files(rv[["faculty-dir"]], pattern = "\\.xlsx$",
                          full.names = TRUE)

      msg <- list()
      count <- 0
      for (f in files) {
        count <- count + 1
        wrn <- tryCatch(verify_professor_excel(f, students),
                        warning = function(e) e)
        if (inherits(wrn, 'warning')) {
          msg[[count]] <- wrn$message
        }
      }

      if (length(msg) == 0) {
        showModal(
          modalDialog(
            "検証が完了しました。",
            size = "l"
          )
        )
      } else {
        showModal(
          modalDialog(
            !!!purrr::map(msg, p, class = "mb-0"),
            size = "xl",
            title = "Warnings"
          )
        )
      }

    }) |>
      bindEvent(
        input[["faculty-verify"]]
      )

    observe({
      req(rv[["students-dir"]], rv[["faculty-list"]])

      faculty <- readxl::read_xlsx(rv[["faculty-list"]])
      files <- list.files(rv[["students-dir"]], pattern = "\\.xlsx$",
                          full.names = TRUE)

      msg <- list()
      count <- 0
      for (f in files) {
        count <- count + 1
        wrn <- tryCatch(verify_student_excel(f, faculty),
                        warning = function(e) e)
        if (inherits(wrn, 'warning')) {
          msg[[count]] <- wrn$message
        }
      }

      if (length(msg) == 0) {
        showModal(
          modalDialog(
            "検証が完了しました。",
            size = "l"
          )
        )
      } else {
        showModal(
          modalDialog(
            !!!purrr::map(msg, p, class = "mb-0"),
            size = "xl",
            title = "Warnings"
          )
        )
      }

    }) |>
      bindEvent(
        input[["students-verify"]]
      )
  })
}
