#' sim_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_config_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    style = "margin: 0; height: 100%",

    column(
      width = 5,
      style = "height: 100%; padding-bottom: 65px",
      # material_card(
        mod_sim_table_ui("sim_table")
      # )
    ),
    column(
      width = 4,
      style = "height: 100%; padding: 0 0 65px 0",
      mod_sim_map_ui("sim_map"),
      mod_sim_map_filter_ui("sim_map_filter")
    ),
    column(
      width = 3,
      style = "height: 100%; padding: 0 0 0 0",
      # material_card(
        mod_sim_school_ui("sim_school")
      # )
    )
  )

}

#' sim_config Server Functions
#'
#' @noRd
mod_sim_config_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sim_config_ui("sim_config_1")

## To be copied in the server
# mod_sim_config_server("sim_config_1")
