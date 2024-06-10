rename_student_excel <- function(dir, nc) {
  cols <- the$cols

  # 学生ファイルの名前が長すぎて読み込めないので修正する。
  .student_files <- list.files(dir, "\\.xlsx$")
  .student_before <- .student_files[nchar(.student_files) > (nc + 5)]

  n <- length(.student_before)

  if (n != 0) {
    .student_after <- paste0(substr(.student_before, 1, nc), ".xlsx")
    file.rename(
      file.path(dir, .student_before),
      file.path(dir, .student_after)
    )
    message("Renamed ", n, " files", "\n")
  } else {
    message("...Skipped....", "\n")
  }

  n
}



verify_student_excel <- function(f, faculty) {
  cols <- the$cols

  # 学生作成の Excel ファイルを1つ読む
  x <- readxl::read_excel(f, sheet = 1)

  # 必須列の確認
  .required_cols_st <- c(cols$student$fcname, cols$student$rank)
  nonexistent_col <- is.na(match(.required_cols_st, names(x)))

  if (any(nonexistent_col)) {
    # "必須列がありません。: "
    warning(
      basename(f), "... ",
      "\u5fc5\u9808\u5217\u304c\u3042\u308a\u307e\u305b\u3093\u3002: ",
      paste(.required_cols_st[nonexistent_col], collapse = ", "),
      call. = FALSE
    )

    return(FALSE)
  }

  # 行（教員名）の確認
  f_prof_name <- faculty |> dplyr::pull(cols$admin$fcname)
  x_prof_name <- x |> dplyr::pull(cols$student$fcname)

  nonexistent_row <- is.na(match(f_prof_name, x_prof_name))
  if (any(nonexistent_row)) {
    # "教員が脱落しています: "
    warning(
      basename(f), "... ",
      "\u6559\u54e1\u304c\u8131\u843d\u3057\u3066\u3044\u307e\u3059: ",
      paste(faculty[nonexistent_row, cols$admin$fcname], collapse = ", "),
      call. = FALSE
    )

    return(FALSE)
  }

  message(basename(f), "... ok")
  TRUE
}


verify_professor_excel <- function(f, students) {
  cols <- the$cols

  # 教員作成の Excel ファイルを1つ読む
  x <- readxl::read_excel(f, sheet = cols$seminar_meta$sh_eval)

  # 必須列の確認
  .required_cols_fc <- c(cols$seminar$stid, cols$seminar$point)
  nonexistent_col <- is.na(match(.required_cols_fc, names(x)))
  if (any(nonexistent_col)) {
    # "必須列がありません: "
    warning(
      basename(f), "... ",
      "\u5fc5\u9808\u5217\u304c\u3042\u308a\u307e\u305b\u3093\u3002: ",
      paste(.required_cols_fc[nonexistent_col], sep = ", "),
      call. = FALSE
    )

    return(FALSE)
  }

  # 行（学生）の確認
  nonexistent_row <-
    is.na(match(
      students |> dplyr::pull(cols$admin$stid),
      x |> dplyr::pull(cols$seminar$stid)
    ))

  if (any(nonexistent_row)) {
    # "学生が脱落しています: "
    warning(
      basename(f), "... ",
      "\u5b66\u751f\u304c\u8131\u843d\u3057\u3066\u3044\u307e\u3059: ",
      paste(students[nonexistent_row, cols$admin$stid], sep = ", "),
      call. = FALSE
    )
    return(FALSE)
  }

  message(basename(f), "... ok")
  TRUE
}
