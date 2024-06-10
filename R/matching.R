read_student_excel <- function(f, faculty) {
  cols <- the$cols

  x <- readxl::read_excel(f, sheet = 1)
  if ("logical" == x |>
    dplyr::pull(cols$student$rank) |>
    typeof()) {
    x <- x |>
      dplyr::mutate((!!cols$student$rank) := NA_integer_)
  }

  f_prof_name <- faculty |> dplyr::pull(cols$admin$fcname)
  x_prof_name <- x |> dplyr::pull(cols$student$fcname)

  replacement <- list()
  replacement[[cols$student$rank]] <- 100
  x <- x |> tidyr::replace_na(replacement)
  y <- jitter(100 - x |> dplyr::pull(cols$student$rank), amount = 0.01)
  names(y) <- faculty[match(x_prof_name, f_prof_name), cols$admin$fcid, drop = TRUE]

  # 統一された順序に整列する
  y[faculty |> dplyr::pull(cols$admin$fcid)]
}


read_professor_excel <- function(f, students) {
  cols <- the$cols

  # 教員作成の Excel ファイルを1つ読む
  param <- readxl::read_excel(f,
    sheet = cols$seminar_meta$sh_opt,
    range = cols$seminar_meta$sh_opt_rng
  )

  use_gpa <- param[1, cols$seminar_meta$sh_opt_col, drop = TRUE]
  x <- readxl::read_excel(f, sheet = cols$seminar_meta$sh_eval)

  if (use_gpa) {
    x <- merge(x, students[, c(cols$admin$stid, cols$admin$gpa)],
      by.x = cols$seminar$stid, by.y = cols$admin$stid
    )
    x[, cols$seminar$point] <- x[, cols$seminar$point] + x[, cols$admin$gpa]
  }

  y <- jitter(x |> dplyr::pull(cols$seminar$point), amount = 0.005)
  names(y) <- x |> dplyr::pull(cols$seminar$stid)

  # 統一された順序に整列する
  y[students |> dplyr::pull(cols$admin$stid)]
}


#' 効用を計算する
#'
#' @param student_list path string. Admin-managed list of students and their GPA.
#' @param faculty_list path string. Admin-managed list of faculty.
#' @param dir_student path string. Directory for student preferences.
#' @param dir_faculty path string. Directory for faculty preferences.
#' @param nc integer. Length of registration ID.
#' @param seed integer. Used as a random seed.
#'
#' @return Returns matching results.
#' @export
#'
matching_utils <- function(
    student_list,
    faculty_list,
    dir_student,
    dir_faculty,
    nc = 8,
    seed = NULL) {
  set.seed(seed)
  cols <- the$cols

  # 学生側ファイルの読み込み
  student_data <- list()
  for (f in list.files(dir_student, pattern = "\\.xlsx$", full.names = TRUE)) {
    # ファイル名「学籍番号.xlsx」から学籍番号を取得
    student_i <- substr(tools::file_path_sans_ext(basename(f)), start = 1, stop = nc)

    # Excel ファイルを読み込んでスコアに変換
    x <- read_student_excel(f, faculty_list)
    student_data[[student_i]] <- x
  }

  Student <- do.call(cbind, student_data)
  Student <- Student[faculty_list |> dplyr::pull(cols$admin$fcid), ]


  faculty_data <- list()
  for (f in list.files(dir_faculty, pattern = "\\.xlsx$", full.names = TRUE)) {
    professor_id <- tools::file_path_sans_ext(basename(f))
    x <- read_professor_excel(f, student_list)
    faculty_data[[professor_id]] <- x
  }

  Faculty <- do.call(cbind, faculty_data)
  Faculty <- Faculty[colnames(Student), faculty_list |> dplyr::pull(cols$admin$fcid)]

  list(Student = Student, Faculty = Faculty)
}



#' マッチングを計算する
#'
#' `matchingR::galeShapley.collegeAdmissions()` を使って学生最適なマッチングを計算する。
#'
#' @param util list. [matching_utils()] の結果
#' @param slots integer vector. 各ゼミの定員
#'
#' @return マッチングの結果。列 (`Student`, `Seminar`) を持つ tibble.
#' @export
#'
matching_compute <- function(util, slots) {
  result <- matchingR::galeShapley.collegeAdmissions(
    studentUtils = util$Student,
    collegeUtils = util$Faculty,
    slots = slots,
    studentOptimal = TRUE
  )

  result$match_table <- tibble::tibble(
    Student = rownames(util$Faculty),
    Seminar = colnames(util$Faculty)[result$matched.students]
  )

  result
}


matching_stat <- function(util, result) {

}
