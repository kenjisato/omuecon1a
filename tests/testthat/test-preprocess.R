test_that("verification pass: well-formatted student excel", {
  file <- test_path("samples", "bad", "Admin", "faculty.xlsx")
  faculty <- readxl::read_xlsx(file, 1)


  f <- test_path("samples", "bad", "Students", "STIDok4.xlsx")
  suppressMessages(
    expect_message(ret <- verify_student_excel(f, faculty), regexp = ".*ok.*"),
  )

  expect_true(ret)

  f <- test_path("samples", "bad", "Students", "STIDok5.xlsx")
  suppressMessages(
    expect_message(ret <- verify_student_excel(f, faculty), regexp = ".*ok.*")
  )
  expect_true(ret)
})


test_that("verification fails: ill-formatted student excel", {
  faculty <- readxl::read_xlsx(test_path("samples", "bad", "Admin", "faculty.xlsx"), 1)


  f <- test_path("samples", "bad", "Students", "STIDerr1.xlsx")
  suppressMessages(
    expect_warning(ret <- verify_student_excel(f, faculty), regexp = ".*必須列.*")
  )
  expect_false(ret)

  f <- test_path("samples", "bad", "Students", "STIDerr2.xlsx")
  suppressMessages(
    expect_warning(ret <- verify_student_excel(f, faculty), regexp = ".*必須列.*")
  )
  expect_false(ret)

  f <- test_path("samples", "bad", "Students", "STIDerr3.xlsx")
  suppressMessages(
    expect_warning(ret <- verify_student_excel(f, faculty), regexp = ".*脱落.*")
  )
  expect_false(ret)
})


test_that("verification fails: ill-formatted faculty excel", {
  students <- readxl::read_xlsx(test_path("samples", "bad", "Admin", "students.xlsx"), 1)

  f <- test_path("samples", "bad", "Faculty", "PROFerr1.xlsx")
  suppressMessages(
    expect_warning(ret <- verify_professor_excel(f, students), regexp = ".*必須列.*")
  )
  expect_false(ret)

  f <- test_path("samples", "bad", "Faculty", "PROFerr2.xlsx")
  suppressMessages(
    expect_warning(ret <- verify_professor_excel(f, students), regexp = ".*必須列.*")
  )
  expect_false(ret)

  f <- test_path("samples", "bad", "Faculty", "PROFerr3.xlsx")
  suppressMessages(
    expect_warning(ret <- verify_professor_excel(f, students), regexp = ".*脱落.*")
  )
  expect_false(ret)
})


test_that("verification pass: well-formatted faculty excel", {
  students <- readxl::read_xlsx(test_path("samples", "bad", "Admin", "students.xlsx"), 1)

  f <- test_path("samples", "bad", "Faculty", "PROFok1.xlsx")
  suppressMessages(
    expect_message(ret <- verify_professor_excel(f, students), regexp = ".*ok.*")
  )
  expect_true(ret)

  f <- test_path("samples", "bad", "Faculty", "PROFidA.xlsx")
  suppressMessages(
    expect_message(ret <- verify_professor_excel(f, students), regexp = ".*ok.*")
  )
  expect_true(ret)
})
