setup_excel_names <- function() {
  config_file <- getOption("omuecon1a.config", pkg_file("config.yml"))
  the$cols <- yaml::read_yaml(config_file)

  the$col_types <- list()
  the$col_types$admin_fc <- "text"
  the$col_types$admin_st <- rlang::set_names(
    c("text", "text", "numeric"),
    nm = unlist(the$cols$admin[c("stid", "stname", "gpa")], use.names = FALSE)
  )
  the$col_types$change <- "text"
}


setup_report_names <- function() {
  col_types <- readr::cols(
    step = readr::col_integer(),
    key = readr::col_character(),
    filename = readr::col_character(),
    usage = readr::col_character()
  )
  the$report <- readr::read_csv(pkg_file("ui", "reports.csv"),
    col_types = col_types
  )
}

get_report <- function(step, key) {
  the$report$filename[the$report$step == step & the$report$key == key]
}

setup_checklist <- function() {
  files <- list.files(pkg_file("ui"), pattern = "checklist$")
  the$checklist <- list()
  for (f in files) {
    label <- strsplit(f, "-")[[1]][[1]]
    the$checklist[[label]] <- readLines(pkg_file("ui", f))
  }
}
