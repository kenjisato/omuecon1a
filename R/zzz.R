.onLoad <- function(libname, pkgname) {
  setup_excel_names()
  setup_report_names()
  setup_checklist()

  op <- options()
  op.omuecon1a <- list(
    # omuecon1a.config = pkg_file("config.yml")
  )
  toset <- !(names(op.omuecon1a) %in% names(op))
  if (any(toset)) options(op.omuecon1a[toset])

  if (.Platform$OS.type == "windows") {
    Sys.setlocale(locale = "Japanese_Japan.utf8")
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {

}
