#' simulacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simulacao_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' simulacao Server Functions
#'
#' @noRd 
mod_simulacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_simulacao_ui("simulacao_1")
    
## To be copied in the server
# mod_simulacao_server("simulacao_1")
