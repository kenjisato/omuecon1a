
student_xlsx_randomize <- function(path = NULL) {
  sdir <- if (is.null(path)) {
    isolate(rv[["students-dir"]])
  } else {
    path
  }

  for (f in list.files(sdir)) {
    suffix <- paste(sample(letters, 30, replace = TRUE), collapse = "")
    new_name <- paste0(xfun::sans_ext(f), "-", suffix, ".", xfun::file_ext(f))

    file.rename(
      file.path(sdir, f),
      file.path(sdir, new_name)
    )
  }
}
