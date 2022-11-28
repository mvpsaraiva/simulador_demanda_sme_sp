#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c("      280px         1fr    ",
                 "1fr   table         content",
                 "75px  buttons       content"
      ),
      gridlayout::grid_card(
        "table",
        mod_res_selector_ui("res_selector")
      ),
      gridlayout::grid_card(
        "buttons",
        div(
          style = "margin: auto; padding: auto; height: 100%; display: flex; justify-content: center; align-items: center;",
          downloadButton(ns("download_xlsx"), label = "Exportar resultados")
        )
      ),
      gridlayout::grid_card(
        "content",
        tabsetPanel(
          id = ns("tab_scenario_details"),
          tabPanel(title = "Modificações",
                   mod_res_summary_ui("res_summary")
          ),
          tabPanel(title = "Modificações por Setor",
                   style = "margin: 5px; padding: 5px;",
                   mod_res_summary_area_ui("res_summary_area")
          ),
          tabPanel(title = "Déficit por Setor",
                   style = "margin: 5px; padding: 5px;",
                   mod_res_deficit_setor_ui("res_deficit_setor")
          )
        )
      )
    )
  )
}

#' results Server Functions
#'
#' @noRd
mod_results_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
