#' resultados UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_resultados_ui <- function(id){
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c("    1fr   ",
                 "1fr data  "),
      gridlayout::grid_card
      ("data",
        tabsetPanel(
          tabPanel(title = "Cenários", DT::dataTableOutput(ns("tabela_escolas"))),
          tabPanel(title = "Por Distrito", DT::dataTableOutput(ns("tabela_escolas"))),
          tabPanel(title = "Por Setor", DT::dataTableOutput(ns("tabela_escolas_modificadas"))),
          tabPanel(title = "Por Hexágono")
        )
      )
    )
  )
}

#' resultados Server Functions
#'
#' @noRd
mod_resultados_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_resultados_ui("resultados_1")

## To be copied in the server
# mod_resultados_server("resultados_1")
