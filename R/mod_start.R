#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_start_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' start Server Functions
#'
#' @noRd 
mod_start_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_start_ui("start_1")
    
## To be copied in the server
# mod_start_server("start_1")
