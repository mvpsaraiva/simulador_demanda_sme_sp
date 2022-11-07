#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ## check if database exists, and download it if not
  db_file <- golem::get_golem_options("db_path")
  if (!file.exists(db_file)) {

    if (!dir.exists("data")) {
      dir.create("data")
    }

    asset_file_name = basename(db_file)
    piggyback::pb_download(file = asset_file_name,
                           repo = "mvpsaraiva/simulador_demanda_sme_sp",
                           tag = "v0.0.0.9000",
                           dest = "data")
  }

  ## connection to the database
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    golem::get_golem_options("db_path")
  )

  # Your application server logic
  # mod_estudantes_server("estudantes_1", con)
  # mod_matriculas_server("matriculas_1", con)
  # mod_deficit_server("deficit_1", con)
  # mod_start_server("start_1")
  mod_simulacao_server("simulacao_1", con)
  mod_resultados_server("resultados_1", con)

}
