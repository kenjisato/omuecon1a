# Components
link_juicedown_app <- shiny::tags$a(
  shiny::icon("github"), "juicedownApp",
  href = "https://github.com/kenjisato/juicedownApp",
  target = "_blank"
)
link_juicedown <- shiny::tags$a(
  shiny::icon("github"), "juicedown",
  href = "https://github.com/kenjisato/juicedown",
  target = "_blank"
)


appid <- function() {
  "omuecon1a-matching"
}


#' Launch shiny app
#'
#' @param config character. path to config file, read by `read_config()`.
#'
#' @return Launch the shiny app.
#' @export
#'
up <- function(config = NULL) {
  app_title <- "OMUEcon 専門演習 1A"
  app_theme <-
    bs_theme(version = "5", preset = "yeti")

  read_config(config)

  pageUi <- page_navbar(
    title = app_title,
    theme = app_theme,
    nav_panel("マッチング", mainUi(), value = 1L),
    nav_panel("操作手引", page_fixed("hello"), value = 2L),
    nav_spacer(),
    nav_item(input_dark_mode()),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_juicedown_app),
      nav_item(link_juicedown)
    )
  )

  pageServer <- function(input, output, session) {
    # rv <- reactiveValues()

    #bslib::bs_themer()
    mainServer(rv)
    options_server(rv, input, output, session)

    # stores setup
    shinyStorePlus::setupStorage(appId = appid(), inputs = TRUE)
  }

  shinyApp(
    pageUi,
    pageServer
  )
}
