#' res_deficit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_res_deficit_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' res_deficit Server Functions
#'
#' @noRd 
mod_res_deficit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_res_deficit_ui("res_deficit_1")
    
## To be copied in the server
# mod_res_deficit_server("res_deficit_1")
