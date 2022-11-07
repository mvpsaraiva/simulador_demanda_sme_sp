db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
con <- db_con

DBI::dbListTables(con)


gridlayout::grid_container(
  layout = c(
    "      1fr          1fr          1fr          ",
    "100px header       header       header       ",
    "100px header_antes header_antes header_depois",
    "300px turmas_antes vagas_antes  vagas_depois"
  ),
  gridlayout::grid_card("header", h4(textOutput(ns("nome_escola")))),

  gridlayout::grid_card("header_antes", "Oferta Atual"),
  gridlayout::grid_card("turmas_antes",
                        "Turmas",
                        textInput(inputId = ns("turmas_creche"),
                                  label = "Creche"),
                        textInput(inputId = ns("turmas_pre"),
                                  label = "Pré-escola"),
                        textInput(inputId = ns("turmas_fund_ai"),
                                  label = "Fundamental I"),
                        textInput(inputId = ns("turmas_fund_af"),
                                  label = "Fundamental II")
  ),

  gridlayout::grid_card("vagas_antes",
                        "Vagas",
                        textInput(inputId = ns("vagas_creche"),
                                  label = "Creche"),
                        textInput(inputId = ns("vagas_pre"),
                                  label = "Pré-escola"),
                        textInput(inputId = ns("vagas_fund_ai"),
                                  label = "Fundamental I"),
                        textInput(inputId = ns("vagas_fund_af"),
                                  label = "Fundamental II")
  ),

  gridlayout::grid_card("header_depois", "Oferta Ampliada"),
  gridlayout::grid_card("vagas_depois",
                        "Vagas",
                        textInput(inputId = ns("vagas_creche_depois"),
                                  label = "Creche"),
                        textInput(inputId = ns("vagas_pre_depois"),
                                  label = "Pré-escola"),
                        textInput(inputId = ns("vagas_fund_ai_depois"),
                                  label = "Fundamental I"),
                        textInput(inputId = ns("vagas_fund_af_depois"),
                                  label = "Fundamental II")
  )
)
