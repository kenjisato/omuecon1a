pkg_file <- function(..., pkg = .packageName) {
  system.file(..., package = pkg, mustWork = TRUE)
}

n_checked <- function(x) {
  if (is.null(x)) 0 else length(x)
}

path_exists <- function(path) {

  if (is.null(path)) return(invisible())

  if (file.exists(path)) {
    path
  } else {
    warning("File/directory does not exist: ", path,
            immediate. = TRUE)
    invisible()
  }
}


#' Last n letters
#'
#' @param s character vector.
#' @param n integer.
#'
#' @return character vector. The elements of the result vector are last n letters
#'  of input vector s.
#' @export
#'
#' @examples
#' last_n(c("abc", "wxyz"), 2)
last_n <- function(s, n) {
  stop <- nchar(s)
  start <- stop - n + 1
  substr(s, start, stop)
}


# https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


tryAlert <- function() {

}


tryRead <- function(file, msg, title = "Error!", ...) {

  ddd <- list(...)

  if (!is.null(file) && length(ext <- xfun::file_ext(file)) > 0) {
    reader <- switch(
      ext,
      xlsx = function(x) readxl::read_xlsx(x, col_types = ddd$col_types),
      xls = function(x) readxl::read_xls(x, col_types = ddd$col_types),
      rds = function(x) readRDS(x)
    )

    obj <- try(reader(file), silent = TRUE)
  } else {
    obj <- NULL
  }

  if (inherits(obj, "try-error") || is.null(obj)) {
    shinyalert::shinyalert(
      title = title,
      text = msg
    )
    rlang::return_from(parent.frame(1), NULL)
  }

  obj
}







