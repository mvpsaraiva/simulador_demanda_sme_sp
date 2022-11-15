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
    fluidRow(
      uiOutput(ns("school_details"))
    )
  )
}

#' sim_school Server Functions
#'
#' @noRd
mod_sim_school_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Headline ----------------------------------------------------------------

    output$school_details <- renderUI({
      req(state$school_selected)

      if (state$school_selected == -1) {
        # no school selected
        tagList(
          h4("Nenhuma escola selecionada", class = "tile-headline"),
          # h5(state$dre, class = "tile-subheadline")
        )

      } else {
        # build ui with school details

        escola <- escolas |>
          dplyr::filter(co_entidade == state$school_selected)

        tagList(
          h4(escola$no_entidade, class = "tile-headline"),
          h5(escola$ds_endereco, class = "tile-subheadline")
        )
      }

    })
  })
}

## To be copied in the UI
# mod_sim_school_ui("sim_school_1")

## To be copied in the server
# mod_sim_school_server("sim_school_1")
