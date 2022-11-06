#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # connection to the database
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    golem::get_golem_options("db_path")
  )

  # Your application server logic
  mod_estudantes_server("estudantes_1", con)
  mod_matriculas_server("matriculas_1", con)
  mod_deficit_server("deficit_1", con)
  mod_simulacao_server("simulacao_1", con)

}
