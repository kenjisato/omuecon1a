match_table <- function(util, result, students_xlsx, faculty_xlsx) {
  cols <- the$cols
  df <- list()

  .seminar <- rlang::sym(cols$match$seminar)

  df$main <- result$match_table |>
    dplyr::left_join(
      students_xlsx,
      by = dplyr::join_by(!!cols$match$student == !!cols$admin$stid)
    ) |>
    dplyr::mutate(
      !!cols$main$stid_name := paste(
        last_n(.data[[cols$match$student]], 3),
        .data[[cols$admin$stname]]
      )
    ) |>
    dplyr::left_join(
      faculty_xlsx,
      by = dplyr::join_by(!!cols$match$seminar == !!cols$admin$fcid)
    ) |>
    dplyr::rename(
      !!cols$main$stname := cols$admin$stname,
      !!cols$main$fcname := cols$admin$fcname
    ) |>
    dplyr::group_by(!!.seminar) |>
    dplyr::mutate(!!cols$main$num := dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(-!!cols$admin$gpa)

  df$notfull <- faculty_xlsx |>
    dplyr::mutate(!!cols$notfull$openslots := rowSums(is.na(result$matched.colleges))) |>
    dplyr::select(!!cols$notfull$seminar := !!cols$admin$fcname,
                  !!cols$notfull$openslots) |>
    dplyr::filter(.data[[cols$notfull$openslots]] > 0)

  df$display <-
    df$main |>
    dplyr::select(
      !!cols$moodle$stid := !!cols$match$student,
      !!cols$moodle$seminar := !!cols$main$fcname
    )

  df
}


wide_table <- function(df, faculty_xlsx, slots) {
  cols <- the$cols
  main <- df$main

  ## 通番列を作成
  wide_id <- tibble::tibble(!!cols$wide$num := seq_len(slots))

  ## 教員ごとに列を作成、中身は空
  for (prof in faculty_xlsx[[cols$admin$fcname]]) {
    wide_id[prof] <- ""
  }

  ## 学生名入りと学生名抜きの2パターン作成する
  wide_name <- wide_id

  ## 各教員ごとのマッチ結果で空欄を埋めていく
  for (prof in faculty_xlsx[[cols$admin$fcname]]) {
    matched_students <-
      main |>
      dplyr::filter(.data[[cols$main$fcname]] == prof)

    st_names <-
      matched_students |>
      dplyr::pull(cols$main$stid_name)

    st_id <-
      matched_students |>
      dplyr::pull(cols$match$student)

    wide_name[seq_along(st_names), prof] <- st_names
    wide_id[seq_along(st_id), prof] <- st_id
  }

  list(name = wide_name, id = wide_id)
}


eyeballing <- function(util) {
  util_st_rounded <- 100 - round(util$Student)
  utils::write.csv(
    util_st_rounded,
    file = file.path(rv[["results-dir"]], get_report(2, "stutil"))
  )

  utils::write.csv(
    round(util$Faculty, 2),
    file = file.path(rv[["results-dir"]], get_report(2, "fcutil"))
  )
}


moodle_page <- function(df, outdir, step, template_file = NULL) {
  cols <- the$cols

  if (file.exists(template_file)) {
    template <- readLines(template_file)
  } else {
    template <- readLines(pkg_file("template", template_file))
  }

  dir.create(tdir <- tempfile(pattern = "dir-"))
  on.exit(unlink(tdir), add = TRUE)

  display <- if (step == 4) {
    df$display |>
      dplyr::filter(!startsWith(.data[[cols$moodle$stid]], "*"))
  } else {
    df$display
  }

  rendered <- whisker::whisker.render(
    template,
    data = list(
      result = paste(knitr::kable(display), collapse = "\n"),
      not_full = paste(knitr::kable(df$notfull), collapse = "\n")
    )
  )
  writeLines(rendered, file.path(tdir, get_report(step, "moodle")))

  juicedown::convert(file.path(tdir, get_report(step, "moodle")),
    dir = outdir, clip = FALSE
  )
}



statistics <- function(util, result) {
  cols <- the$cols
  util_st_rounded <- 100 - round(util$Student)

  match_stat <- result$match_table
  match_stat[cols$stat$rank] <- 0L
  local({
    for (i in seq_len(nrow(match_stat))) {
      student <- match_stat[i, cols$match$student] |> dplyr::pull()
      seminar <- match_stat[i, cols$match$seminar] |> dplyr::pull()
      match_stat[i, cols$stat$rank] <<- util_st_rounded[seminar, student]
    }
  })

  ## 100は大きすぎるので減らす
  match_stat[match_stat[cols$stat$rank] == 100, cols$stat$rank] <- 50 # 未入力は50

  # いくつのゼミに順位付けしたか？
  num_ranked_profs <-
    tibble::as_tibble(colSums(util_st_rounded < 100),
      rownames = cols$match$student
    ) |>
    dplyr::rename(!!cols$stat$numranked := "value")

  match_stat <-
    match_stat |>
    dplyr::left_join(num_ranked_profs, by = cols$match$student)

  ## =============================================================================
  ## 【統計情報】チェック_配属ゼミ希望順位.csv
  ## =============================================================================

  readr::write_excel_csv(
    match_stat,
    file = file.path(rv[["results-dir"]], get_report(2, "numranked"))
  )

  ## =============================================================================
  ## 【統計情報】step2_チェック_配属ゼミ希望順位.png
  ## =============================================================================
  .rank <- rlang::sym(cols$stat$rank)
  p <- ggplot2::ggplot(match_stat, ggplot2::aes(x = !!.rank)) +
    ggplot2::geom_bar(fill = "skyblue") +
    ggplot2::theme_linedraw()
  ggplot2::ggsave(file.path(rv[["results-dir"]], get_report(2, "numranked-graph")),
    p,
    dpi = 300, width = 800, height = 480, units = "px"
  )

  ## =============================================================================
  ## 【統計情報】マッチ順位on順位付けの数.png
  ## =============================================================================

  .numranked <- rlang::sym(cols$stat$numranked)
  q <- ggplot2::ggplot(match_stat, ggplot2::aes(x = !!.numranked, y = !!.rank)) +
    ggplot2::geom_jitter(width = 0.05, height = 0.05, size = 3, alpha = 0.4) +
    ggplot2::xlab("Number of seminars ranked") +
    ggplot2::ylab("Ranking given to \nassigned seminar") +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggplot2::scale_y_reverse(breaks = integer_breaks()) +
    ggplot2::theme_linedraw()

  ggplot2::ggsave(
    file.path(
      rv[["results-dir"]],
      get_report(2, "numranked-vs-ranked-graph")
    ),
    q,
    dpi = 300, width = 800, height = 600, units = "px"
  )
}
