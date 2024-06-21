choose_dir <- function(caption = "select a directory") {
  dir <- tryCatch(
    error = function(e) {
      message("You are not on Windows. Attempting RStudio API...\n")
      rstudioapi::selectDirectory(caption)
    },
    utils::choose.dir(caption = caption)
  )
}


#' Prepare project directory
#'
#' Extract sample files, the recommended structure for the matching project.
#' This function is intended to be used in the interactive shell.
#'
#' @param dir character. The name of the directory to extract sample files.
#' @param parent character. The name of the enclosing directory. Directory
#'   picker will open when unspecified (or NULL).
#'
#' @return Invisibly returns the path to the project directory.
#' @export
#'
prep <- function(dir = format(Sys.Date(), "matching%y"), parent = NULL) {

  parent <- if (is.null(parent)) {
    choose_dir()
  } else {
    path.expand(parent)
  }
  if (is.null(parent)) {
    stop("parent is not specified.")
  }

  to <- file.path(parent, dir)

  if (dir.exists(to)) {
    stop("Target directory already exists: ", to)
  }

  tdir <- tempfile(pattern = "dir")
  dir.create(tdir)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  file.copy(pkg_file("project"), tdir, recursive = TRUE)
  file.rename(file.path(tdir, "project", "omuecon1a.Rproj"),
              file.path(tdir, "project", xfun::with_ext(dir, "Rproj")))

  writeLines(
    whisker::whisker.render(
      readLines(pkg_file("project", "project.yml")),
      list(project_dir = to)
    ),
    con = file.path(tdir, "project", "project.yml")
  )

  file.rename(file.path(tdir, "project"), file.path(tdir, dir))

  file.copy(file.path(tdir, dir), parent, recursive = TRUE)

  message("Created sample project in \n", "  ", to)
  invisible(to)
}
