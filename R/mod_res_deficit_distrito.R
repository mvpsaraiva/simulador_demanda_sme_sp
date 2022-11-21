#' res_deficit_distrito UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_deficit_distrito_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' res_deficit_distrito Server Functions
#'
#' @noRd
mod_res_deficit_distrito_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_res_deficit_distrito_ui("res_deficit_distrito_1")

## To be copied in the server
# mod_res_deficit_distrito_server("res_deficit_distrito_1")
