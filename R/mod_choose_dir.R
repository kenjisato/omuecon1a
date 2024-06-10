# Choose a directory ----

choose_dir_ui <- function(id, face, msg, tip = NULL) {
  ns <- NS(id)

  text_input <- shinyjs::disabled(
    textInput(ns("txt"), label = NULL, width = "100%")
  )
  if (!is.null(tip)) {
    text_input <- tooltip(text_input, tip)
  }

  layout_columns(
    shinyFiles::shinyDirButton(
      ns("btn"),
      label = face,
      title = msg
    ),
    text_input,
    col_widths = c(2, 10)
  )
}

choose_dir_server <- function(id, rv, root_id = NULL) {
  moduleServer(id, function(input, output, session) {
    base_dir <- reactiveVal()
    dir_path <- reactiveVal()

    if (is.null(root_id)) {
      base_dir(c(home = path.expand("~")))
    } else {
      observe({
        req(rv[[root_id]])
        base_dir(c(wd = rv[[root_id]]))
      })
    }

    observe({
      req(base_dir())
      shinyFiles::shinyDirChoose(input, "btn", roots = base_dir())
    })

    observe({
      dir_path(input$txt)
      rv[[id]] <- input$txt
    }) |>
      bindEvent(
        input$txt #   # When label is updated.
      )

    observe({
      req(base_dir())
      if (length(input$btn) > 1) {
        path_str <- shinyFiles::parseDirPath(roots = base_dir(), input$btn)
        updateTextInput(session, inputId = "txt", value = path_str)
      }
    }) |>
      bindEvent(
        input$btn # When button is clicked.
      )

    return(dir_path)
  })
}



choose_dir_app <- function() {
  ui <- page_fluid(
    choose_dir_ui("root", "btn", "root"),
    choose_dir_ui("sub", "btn", "subdir")
  )

  server <- function(input, output) {
    rv <- reactiveValues()
    root <- choose_dir_server("root", rv)
    choose_dir_server("sub", rv, "root")
  }

  shinyApp(ui, server)
}
