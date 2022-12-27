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
              mod_sim_map_ui("sim_map")
          )
        ),
        gridlayout::grid_card(
          "details",
          mod_sim_school_ui("sim_school")
        ),
        gridlayout::grid_card(
          "run",
          div(
            style = "margin: auto; padding: auto; height: 100%; display: flex; justify-content: center; align-items: center;",
            actionButton(ns("btn_run_simulation"), "Rodar Simulação >>>")
          )
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
      # consolidar elementos necessários à simulação em uma lista
      cenario <- list(
        id = -1,
        data_criacao = Sys.time(),
        nome = state$edit_scenario$name, #  input$cenario_nome,
        autor = state$edit_scenario$author, #  input$cenario_autor,
        descricao = state$edit_scenario$description, #  input$cenario_descricao,
        escolas_mod = state$school_mod,
        escolas = escolas,
        populacao = populacao_por_hex,
        ttm = ttm,
        hexgrid = hexgrid
      )

      # calcular demanda educacional com base no cenario
      cenario_res <- create_new_scenario(cenario)

      # persistir cenário no banco de dados
      persist_scenario(state$db_con, cenario_res)

      # limpar memória e UI
      state$school_mod <- novo_escolas_mod_vazio()
      state$edit_scenario = list(
        name = "",
        author = "",
        description = ""
      )
      state$new_scenario = state$new_scenario + 1

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

