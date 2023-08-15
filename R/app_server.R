#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # mapbox token
  token = golem::get_golem_options("mapbox_token")
  mapdeck::set_token(token)

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
    selected_dres = NULL,
    selected_districts = NULL,
    state = list(id = app_states$STATE_NOTHING_SELECTED, store = list()),
    config = list(selected_dres = NULL,
                  selected_districts = NULL,
                  map_shape = NULL,
                  map_data = NULL,
                  unidade_espacial = "setores_sme",
                  mapa_variavel = "demanda",
                  mapa_etapa = c("creche", "pre", "anos_iniciais", "anos_finais"),
                  mapa_rede = "publica",
                  mapa_ano = 2035,
                  anos = c(2020, 2035),
                  mapa_exibir_escolas = FALSE
                  ),

    centroid = c(-46.64806, -23.64986), # c(-46.63306176720343, -23.548164364465265),

    school_selected = -1,
    edit_school = list(),

    scenario_selected = NA_integer_,
    edit_scenario = list(
      name = "",
      author = "",
      description = ""
    ),

    new_scenario = 0,

    school_mod = novo_escolas_mod_vazio(),
    school_add = carrega_novas_escolas(con),
    add_school = list(),
    new_school = 0,
    added_school_selected = c(),

    window_height = 800,
    map_id = NULL,

    db_con = con
  )

  # keyring::keyring_unlock(password = "frentepopulardajudeia")

  # res_auth <- shinymanager::secure_server(
  #   check_credentials = shinymanager::check_credentials(
  #     "data/users.sqlite",
  #     passphrase = keyring::key_get("R-shinymanager-key", "tassadar")
  #   )
  # )

  # update the app state when browser window is re-sized
  observeEvent(input$window_size, app_state$window_height <- input$window_size$height)

  # Your application server logic

  # Diagnostics
  mod_diagnostics_server("diagnostics", app_state)
  mod_diag_config_server("diag_config", app_state)
  mod_diag_map_server("diag_map_present", app_state, 1)
  mod_diag_map_server("diag_map_future", app_state, 2)

  # Simulation
  mod_simulation_server("simulation", app_state)
  mod_sim_table_server("sim_table", app_state)
  mod_sim_map_server("sim_map", app_state)
  mod_sim_school_server("sim_school", app_state)
  mod_sim_edit_school_server("sim_edit_school", app_state)
  mod_sim_add_school_server("sim_add_school", app_state)
  mod_sim_run_server("sim_run", app_state)

  mod_results_server("results", app_state)
  mod_res_selector_server("res_selector", app_state)
  mod_res_selector_server("res_selector_load", app_state)
  mod_res_summary_server("res_summary", app_state)
  mod_res_summary_area_server("res_summary_area", app_state)
  mod_res_deficit_setor_server("res_deficit_setor", app_state)
}
