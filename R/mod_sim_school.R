#' sim_school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_school_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' sim_school Server Functions
#'
#' @noRd
mod_sim_school_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sim_school_ui("sim_school_1")

## To be copied in the server
# mod_sim_school_server("sim_school_1")
