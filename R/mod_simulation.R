#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulation_ui <- function(id){
  ns <- NS(id)
    tagList(
      gridlayout::grid_container(
        layout = c("      1fr    1fr  300px    ",
                   "1fr   table  map details",
                   "75px  table  map run    "
        ),
        gridlayout::grid_card(
          "table",
          mod_sim_table_ui("sim_table")
        ),
        gridlayout::grid_card(
          "map",
          tagList(
            mod_sim_map_ui("sim_map"),
            mod_sim_map_filter_ui("sim_map_filter")
          )
        ),
        gridlayout::grid_card(
          "details",
          mod_sim_school_ui("sim_school")
        ),
        gridlayout::grid_card(
          "run",
          actionButton(ns("btn_run_simulation"), "Rodar Simulação >>>")
        )

      )

    )

}

#' simulation Server Functions
#'
#' @noRd
mod_simulation_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$btn_run_simulation, {

      showModal(modalDialog(
        size = "l",
        easyClose = TRUE,
        mod_sim_run_ui("sim_run"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_run"), "Rodar")
        )
      ))
    })

    # Rodar simulação ---------------------------------------------------------

    observeEvent(input$btn_run, {

      showModal(
        modalDialog(
          title = "Operação em andamento",
          "Aguarde enquanto o novo cenário é processado. Isto pode levar alguns minutos.",
          footer = NULL
        )
      )

      # criar novo cenário de simulação
      # dados:
      # hexgrid
      # populacao_por_hex
      # escolas
      # ttm

      # hexgrid <- read_sf_from_db(db_con, "hexgrid")
      #
      # populacao_por_hex <- DBI::dbReadTable(db_con, "populacao_por_hex") |>
      #   dplyr::filter(ano %in% c(2020, 2035, 2045))
      #
      # escolas_df <- DBI::dbReadTable(db_con, "escolas")
      #
      # travel_times <- DBI::dbReadTable(db_con, "travel_times")

      cenario <- list(
        id = uuid::UUIDgenerate(),
        data_criacao = Sys.time(),
        nome = input$cenario_nome,
        autor = input$cenario_autor,
        descricao = input$cenario_descricao,
        escolas_mod = modificacoes$escolas,
        escolas = escolas_df,
        populacao = populacao_por_hex,
        ttm = travel_times,
        hexgrid = hexgrid
      )

      # calcular demanda educacional com base no cenario
      cenario_res <- create_new_scenario(cenario)

      # persistir cenário no banco de dados
      persist_scenario(db_con, cenario_res)

      # escolas_mod = data.frame(co_entidade = 35000024,
      #                          no_entidade = "GAVIAO PEIXOTO BRIGADEIRO",
      #                          orig_mat_creche = 10,
      #                          nova_mat_creche = 20,
      #
      #                          orig_mat_pre = 10,
      #                          nova_mat_pre = 40,
      #
      #                          orig_mat_fund_ai = 10,
      #                          nova_mat_fund_ai = 50,
      #
      #                          orig_mat_fund_af = 10,
      #                          nova_mat_fund_af = 60
      # )


      # limpar memória e UI
      rm(hexgrid)
      rm(populacao_por_hex)
      rm(escolas_df)
      rm(travel_times)

      modificacoes$escolas <- novo_escolas_mod_vazio()

      # updateTabsetPanel(session, "tab_simulacao", selected = "Escolas")

      removeModal()

      showModal(
        modalDialog(
          title = "Operação concluída",
          "Cenário de simulação calculado com sucesso. Tabelas e mapas gerados podem ser vistos na aba 'Resultados'.",
          footer = modalButton("Fechar")
        )
      )


    })

  })
}

