#' scholl_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scholl_editor_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' scholl_editor Server Functions
#'
#' @noRd
mod_scholl_editor_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



  })
}

## To be copied in the UI
# mod_scholl_editor_ui("scholl_editor_1")

## To be copied in the server
# mod_scholl_editor_server("scholl_editor_1")
