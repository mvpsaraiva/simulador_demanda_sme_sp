#' visualizacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualizacao_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' visualizacao Server Functions
#'
#' @noRd 
mod_visualizacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_visualizacao_ui("visualizacao_1")
    
## To be copied in the server
# mod_visualizacao_server("visualizacao_1")
