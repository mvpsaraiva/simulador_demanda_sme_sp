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


  # initialise the app state with the default STATE_NOTHING_SELECTED
  app_state <- reactiveValues(
    dre_district = app_states$INITIAL_DRE,
    state = list(id = app_states$STATE_NOTHING_SELECTED, store = list()),
    school_selected = -1,
    # state = list(id = STATE_NOTHING_SELECTED, store = list()),
    # direction = INITIAL_DIRECTION,
    # region = INITIAL_REGION,
    window_height = 800,
    # d_commute = NULL,
    map_id = NULL,
    # data_source = INITIAL_DATA_SOURCE

    db_con = con
  )

  # update the app state when browser window is re-sized
  observeEvent(input$window_height, app_state$window_height <- input$window_height)

  # Your application server logic
  # mod_estudantes_server("estudantes_1", con)
  # mod_matriculas_server("matriculas_1", con)
  # mod_deficit_server("deficit_1", con)
  # mod_start_server("start_1")

  mod_simulation_server("simulation", app_state)
  mod_sim_config_server("sim_config", app_state)
  mod_sim_table_server("sim_table", app_state)
  mod_sim_map_server("sim_map", app_state)
  mod_sim_map_filter_server("sim_map_filter", app_state)
  mod_sim_stats_server("sim_stats", app_state)
  mod_sim_school_server("sim_school", app_state)
#
#   mod_simulation_server("simulacao", con)
#   mod_simulation_server("simulacao", con)
#
#   mod_simulacao_server("simulacao_1", con)
#   mod_resultados_server("resultados_1", con)

}
