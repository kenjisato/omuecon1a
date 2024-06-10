# Choose a file ----

choose_file_ui <- function(id, face, msg, value = NULL) {
  ns <- NS(id)
  allow_inspect <- c("xlsx")

  layout_columns(
    shinyFiles::shinyFilesButton(
      ns("btn"),
      label = face,
      title = msg,
      multiple = FALSE
    ),
    shinyjs::disabled(textInput(ns("txt"), label = NULL, value = value)),
    actionButton(ns("inspect"),
      disabled = TRUE,
      label = NULL, icon = icon("search"), class = "btn-light"
    ),
    col_widths = c(2, 9, 1)
  )
}


choose_file_server <- function(id, rv, root_id = NULL, ...) {
  allow_inspect <- c("xlsx")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    base_dir <- reactiveVal()

    if (is.null(root_id)) {
      base_dir(c(home = path.expand("~")))
    } else {
      observe({
        req(rv[[root_id]])
        base_dir(c(wd = rv[[root_id]]))
      })
    }

    file_path <- reactiveVal()
    file_ext <- reactive(xfun::file_ext(input$txt))

    # Is file inspect()-able?
    observe({
      req(file_ext())
      updateActionButton(
        inputId = "inspect",
        disabled = !(file_ext() %in% allow_inspect)
      )
    })

    # register the dir select button
    observe({
      validate(need(base_dir(), "dir not set"))
      shinyFiles::shinyFileChoose(
        input, "btn",
        roots = base_dir(), ...
      )
    })

    # the dir select button is clicked
    observe({
      validate(
        need("files" %in% names(input$btn), "File not ready")
      )
      file_path(shinyFiles::parseFilePaths(base_dir(), input$btn)$datapath[[1]])
      updateTextInput(session, inputId = "txt", value = file_path())
    }) |>
      bindEvent({
        input$btn
      })

    # when the text is updated from outside (typically with indexedbd)
    observe({
      file_path(input$txt)
      rv[[id]] <- input$txt
    }) |>
      bindEvent(
        input$txt
      )

    # when the file ext is xlsx
    output$table <- DT::renderDataTable({
      req(input$txt)
      validate(
        need(file_ext() == "xlsx", "need xlsx file")
      )

      df <- readxl::read_xlsx(input$txt)
      DT::datatable(
        df,
        options = list(pageLength = 10, paging = TRUE, searching = TRUE),
        rownames = FALSE, selection = "single",
      )
    })

    observe({
      req(input$txt)
      showModal(
        modalDialog(
          DT::dataTableOutput(ns("table")),
          size = "l"
        )
      )
    }) |>
      bindEvent(input$inspect)

    return(file_path)
  })
}




choose_file_app <- function() {
  rv <- reactiveValues()

  ui <- page_fluid(
    choose_dir_ui("choose_dir", "dir", msg = "choose a dir"),
    choose_file_ui("choose_st", "btn", "choose a file")
  )

  server <- function(input, output) {
    choose_dir_server("choose_dir", rv)
    choose_file_server("choose_st", rv, "choose_dir", filetype = "xlsx")
  }

  shinyApp(ui, server)
}
