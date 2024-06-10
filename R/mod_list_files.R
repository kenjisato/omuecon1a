list_files_ui <- function(id, buttons = NULL, .tag = div) {
  ns <- NS(id)

  layout_columns(
    .tag(
      buttons
    ),
    .tag(
      verbatimTextOutput(ns("files"), placeholder = TRUE),
      actionButton(ns("refresh"), NULL, icon = shiny::icon("refresh"),
                   class = "btn-sm", width = "100%", size = "xs")
    ),
    .tag(
      textOutput(ns("nexists")),
      textOutput(ns("nall"))
    ),
    col_widths = c(2, 5, 5)
  )
}


list_files_server <- function(id, rv, root_id, pattern = NULL, book = NULL) {
  moduleServer(id, function(input, output, session) {
    files <- reactiveVal()

    observe({
      files(list.files(rv[[root_id]], pattern = pattern))
    })

    output$files <- renderText(files(), sep = "\n")

    observe({
      files(list.files(rv[[root_id]], pattern = pattern))
      output$files <- renderText(files(), sep = "\n")
    }) |>
      bindEvent(
        input[["refresh"]]
      )


    output$nexists <- renderText(paste("ファイル数: ", length(files())))

    observe({
      req(rv[[book]])
      book_xl <- readxl::read_xlsx(isolate(rv[[book]]))
      output$nall <- renderText(paste("登録数: ", nrow(book_xl)))
    })

    return(files)
  })
}


list_files_app <- function() {
  rv <- reactiveValues()

  ui <- page_fluid(
    choose_dir_ui("choose_dir", "dir", msg = "choose a dir"),
    list_files_ui("list_st", .tag = div_)
  )

  server <- function(input, output) {
    choose_dir_server("choose_dir", rv)
    list_files_server("list_st", rv, "choose_dir", pattern = "\\.xlsx$")
  }

  shinyApp(ui, server)
}
