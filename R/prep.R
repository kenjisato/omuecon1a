choose_dir <- function(caption = "select a directory") {
  tryCatch(
    error = function(e) {
      message("You are not on Windows. Attempting RStudio API...")
      rstudioapi::selectDirectory(caption)
    },
    utils::choose.dir(caption = caption)
  )

}


#' Prepare project directory
#'
#' Extract sample files, where the matching project begins.
#' This function is intended to be used in the interactive shell.
#'
#' @param dir character. The name of the directory to extract sample files.
#'
#' @return Invisibly returns the path to the project directory.
#' @export
#'
#' @examples
prep <- function(dir = format(Sys.Date(), "matching%y")) {
  parent_dir <- choose_dir()
  exdir <- file.path(parent_dir, dir)
  zipfile <- pkg_file("zip", "matching-project.zip")
  utils::unzip(zipfile, exdir = exdir, overwrite = FALSE)
  message("Extracted sample files in \n", "  ", exdir)
  invisible(exdir)
}
