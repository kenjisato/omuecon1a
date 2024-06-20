test_that("matching_table works", {

  root_dir <- test_path("intermediates", "case_1")

  students_xlsx <- readxl::read_xlsx(file.path(root_dir, "students.xlsx"),
                                     col_types = the$col_types$admin_st)
  faculty_xlsx <- readxl::read_xlsx(file.path(root_dir, "faculty.xlsx"),
                                    col_types = the$col_types$admin_fc)
  matching_result <- readRDS(file.path(root_dir, "step2.rds"))

  util <- matching_result$util
  result <- matching_result$result

  expected <- readRDS(file.path(root_dir, "expected.rds"))

  expect_equal(
    match_table(util, result, students_xlsx, faculty_xlsx),
    expected
  )

})
