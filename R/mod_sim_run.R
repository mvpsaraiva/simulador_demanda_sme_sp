#' sim_run UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sim_run_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sim_run Server Functions
#'
#' @noRd 
mod_sim_run_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sim_run_ui("sim_run_1")
    
## To be copied in the server
# mod_sim_run_server("sim_run_1")
