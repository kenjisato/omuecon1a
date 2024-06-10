# UI functions ----

choose_folder_ns <- function(ns, key, face, msg, tip = NULL) {

  text_input <- shinyjs::disabled(
    textInput(paste0(ns(key), "-txt"), label = NULL, width = "100%")
  )
  if (!is.null(tip))
    text_input <- tooltip(text_input, tip)

  layout_columns(
    shinyFiles::shinyDirButton(
      paste0(ns(key), "-btn"),
      label = face,
      title = msg
    ),
    text_input,
    col_widths = c(2, 10)
  )
}

choose_file_ns <- function(ns, key, face, msg, filetype = "xlsx") {

  layout_columns(
    shinyFiles::shinyFilesButton(
      paste0(ns(key), "-btn"),
      label = face,
      title = msg,
      multiple = FALSE
    ),
    shinyjs::disabled(textInput(paste0(ns(key), "-txt"), label = NULL)),
    actionButton(paste0(ns(key), "-inspect"), disabled = (filetype != "xlsx"),
                 label = NULL, icon = icon("search"), class = "btn-light"),
    col_widths = col_widths <- c(2, 9, 1)
  )
}


# Server functions ----

dirChooseServer <- function(id, key, proj_root = NULL) {

  inner_id <- paste0(id, "-", key)
  input_id <- "btn"
  output_id <- "txt"
  home_dir <- c(home = path.expand("~"))

  root_server <- function(input, output, session) {

    observe({
      shinyFiles::shinyDirChoose(input, input_id, roots = home_dir)
    })

    path <- reactiveVal()

    observe({
      req(input[[output_id]])
      path(input[[output_id]])
    })

    observeEvent(input[[input_id]], {
      validate(
        need(input[[input_id]], "フォルダが選択されていません。"),
        need(is.vector(input[[input_id]]), "対応していない形式です。")
      )
      path(shinyFiles::parseDirPath(home_dir, input[[input_id]]))
      updateTextInput(session, inputId = output_id, value = path())
    })

    return(path)
  }

  other_server <- function(input, output, session) {

    root <- home_dir
    observe({
      root <<- c(wd = proj_root())
    })
    path <- reactiveVal()

    observe({
      req(input[[output_id]])
      path(input[[output_id]])
    })

    observe({
      validate(
        need(proj_root(), "proj_root not set.")
      )
      shinyFiles::shinyDirChoose(input, input_id, roots = root)
    })

    observeEvent(input[[input_id]], {
      validate(
        need(input[[input_id]], "フォルダが選択されていません。"),
        need(is.vector(input[[input_id]]), "対応していない形式です。")
      )
      path(shinyFiles::parseDirPath(root, input[[input_id]]))
      updateTextInput(
        session, inputId = output_id,
        value = path()
      )
    })

    return(path)
  }

  if (key == "root") {
    moduleServer(inner_id, root_server)
  } else {
    moduleServer(inner_id, other_server)
  }

}


#' ファイルを選択する
inputDataServer <- function(id, key, reactive_dir, filetype = "xlsx") {

  inner_id <- paste0(id, "-", key)
  input_id <- "btn"
  output_id <- "txt"
  inspect <- "inspect"

  moduleServer(inner_id, function(input, output, session) {
    ns <- session$ns

    path <- reactiveVal()

    observe({
      validate(
        need(reactive_dir(), "dir not set")
      )
      shinyFiles::shinyFileChoose(input, input_id, roots = c(wd = reactive_dir()),
                                  filetypes = filetype)
    })

    observeEvent(input[[input_id]], {
      validate(
        need("files" %in% names(input[[input_id]]), "File not ready")
      )
      path(shinyFiles::parseFilePaths(c(wd = reactive_dir()),
                                      input[[input_id]])$datapath[[1]])
      updateTextInput(
        session, inputId = output_id,
        value = path()
      )
    })

    if (filetype == "xlsx") {
      output$table <- DT::renderDataTable({
        req(input[[output_id]])
        df <- readxl::read_xlsx(input[[output_id]])
        DT::datatable(df,
                      options = list(
                        pageLength = 10, paging = TRUE, searching = TRUE
                      ),
                      rownames = FALSE, selection = "single",
        )
      })

      observe({
        req(input[[output_id]])
        showModal(
          modalDialog(DT::dataTableOutput(ns("table")), size = "l")
        )
      }) |>
        bindEvent(input[["inspect"]])
    }

    return(path)
  })
}


#' ファイル一覧等を表示する
inputFilesServer <- function(id, key = c("faculty", "students")) {
  key <- match.arg(key)

  inner_id <- paste0(id, "-", key)
  path_id <- "txt"
  verbtxt_id <- "files"
  counttxt_id <- "file-count"
  refresh_id <- "refresh"

  moduleServer(inner_id, function(input, output, session) {

    files <- reactiveVal()
    observe({
      files(list.files(input[[path_id]]))
    })

    output[[verbtxt_id]] <- renderText(files(), sep = "\n")
    output[[counttxt_id]] <- renderText(paste("ファイル数:", length(files())))

    observe({
      files(list.files(input[[path_id]]))
      output[[verbtxt_id]] <- renderText(files(), sep = "\n")
    }) |>
      bindEvent(input[[refresh_id]])

    return(files)
  })
}

