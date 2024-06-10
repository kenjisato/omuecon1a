## mainUi.R

mainUi <- function() {
  tagList(
    shinyjs::useShinyjs(),
    shinyStorePlus::initStore(),
    includeCSS(pkg_file("www", "css", "style.css")),
    page_fillable(
      id = "main-panel",
      navset_card_pill(
        step0_ui(),
        step1_ui(),
        step2_ui(),
        step3_ui(),
        step4_ui(),
        nav_spacer(),
        option_ui()
      )
    )
  )
}
