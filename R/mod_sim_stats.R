#' sim_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_stats_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' sim_stats Server Functions
#'
#' @noRd
mod_sim_stats_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sim_stats_ui("sim_stats_1")

## To be copied in the server
# mod_sim_stats_server("sim_stats_1")
