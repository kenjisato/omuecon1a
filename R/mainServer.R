# Server functions ----
mainServer <- function(rv) {
  # Step 0 (初期設定) -----
  choose_dir_server("root-dir", rv)

  child_dir <- c(
    "admin-dir",
    "faculty-dir",
    "students-dir",
    "results-dir"
  )

  for (child in child_dir) {
    choose_dir_server(child, rv, "root-dir")
  }

  # Step 1 (データ収集) ----
  choose_file_server("faculty-list", rv, "admin-dir")
  choose_file_server("students-list", rv, "admin-dir")

  list_files_server("faculty-xlsx", rv, "faculty-dir", book = "faculty-list")
  stxls <-
    list_files_server("students-xlsx", rv, "students-dir", book = "students-list")

  step1_server(files = stxls)

  # Step 2 (マッチング) ----
  # No data supplied in this panel..
  step2_server()

  # Step 3 (修正) ----
  choose_file_server("step2-rds", rv, "results-dir", filetype = "rds")
  choose_file_server("change-xlsx", rv, "admin-dir", filetype = "xlsx")
  step3_server()

  # Step 4 (確定) ----
  choose_file_server("step3-rds", rv, "results-dir", filetype = "rds")
  choose_file_server("gakuen-xlsx", rv, "admin-dir", filetype = "xlsx")
  step4_server()

  # Variables
  action_server(
    "step2", step2_action, rv
  )
  action_server(
    "step3", step3_action, rv
  )

  action_server(
    "step4", step4_action, rv
  )

}
