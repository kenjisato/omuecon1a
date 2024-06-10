step0_ui <- function() {
  nav_panel(
    "0. 準備",
    value = "step0",
    page_fixed(
      div_(
        # btn_root
        h5("親フォルダ"),
        p("プロジェクトフォルダ"),
        choose_dir_ui("root-dir", "選択", "ルートフォルダを選んでください。"),
        h5("サブフォルダ"),
        # btn_admin
        p("管理者作成データ"),
        choose_dir_ui("admin-dir", "選択", "管理作成データ用フォルダを選んでください。"),

        # btn_faculty
        p("教員提出データ"),
        choose_dir_ui("faculty-dir", "選択", "教員提出データ用フォルダを選んでください。"),

        # btn_student
        p("学生提出データ"),
        choose_dir_ui("students-dir", "選択", "学生提出データ用フォルダを選んでください。"),

        # btn_result
        p("出力先"),
        choose_dir_ui("results-dir", "選択", "出力保存フォルダを選んでください。"),

        # slots
        h5("ゼミ定員"),
        textInput(
          "slots",
          label = NULL,
          value = 10,
          width = "100%"
        )
      )
    )
  )
}
