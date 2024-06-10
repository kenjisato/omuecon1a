test_that("read student excel properly", {
  faculty <- readxl::read_xlsx(test_path("samples", "good", "Admin", "faculty.xlsx"), 1)

  file <- test_path("samples", "good", "Students", "STIDst1.xlsx")
  expectation <- c("PROFidA" = 98, "PROFidB" = 99, "PROFidC" = 0)
  expect_equal(read_student_excel(file, faculty), expectation, tolerance = 0.01)


  file <- test_path("samples", "good", "Students", "STIDst2.xlsx")
  expectation <- c("PROFidA" = 99, "PROFidB" = 98, "PROFidC" = 97)
  expect_equal(read_student_excel(file, faculty), expectation, tolerance = 0.01)

  file <- test_path("samples", "good", "Students", "STIDst6.xlsx")
  expectation <- c("PROFidA" = 97, "PROFidB" = 98, "PROFidC" = 99)
  expect_equal(read_student_excel(file, faculty), expectation, tolerance = 0.01)
})


test_that("read professor excel properly", {
  students <- readxl::read_xlsx(test_path("samples", "good", "Admin", "students.xlsx"), 1)

  file <- test_path("samples", "good", "Faculty", "PROFidA.xlsx")
  gpa <- c(
    STIDst1 = 3, STIDst2 = 3, STIDst3 = 2,
    STIDst4 = 2, STIDst5 = 1, STIDst6 = 1
  )
  evaluation <- c(
    STIDst1 = 70, STIDst2 = 60, STIDst3 = 50,
    STIDst4 = 40, STIDst5 = 30, STIDst6 = 20
  )
  suppressMessages(
    ret <- read_professor_excel(file, students)
  )
  expect_equal(ret, evaluation + gpa, tolerance = 0.005)
})
