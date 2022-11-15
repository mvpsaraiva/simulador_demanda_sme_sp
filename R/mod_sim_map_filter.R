#' sim_map_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_map_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(
      div(
        style = "float: right",
        shinyWidgets::pickerInput(ns("dre"), label = NULL, choices = NULL, width = "200px")
      ),
      top = "0", right = "0", width = "100%",
      style = "padding: 0 7px 0 20px; margin-top: 6px;"
    )
  )
}

#' sim_map_filter Server Functions
#'
#' @noRd
mod_sim_map_filter_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      # List of DRE for filtering
      dre_list <- setores$nm_dre |>
        unique() |>
        sort()

      # combine both lists
      choices <- as.list(c(app_states$INITIAL_DRE, dre_list))

      # update widget
      shinyWidgets::updatePickerInput(session, "dre", choices = choices, selected = app_states$INITIAL_DRE)
    })

    observeEvent(input$dre, {
      state$dre <- input$dre
      state$state <- list(
        id = app_states$STATE_NOTHING_SELECTED,
        store = list()
      )
    })
  })

}


## To be copied in the UI
# mod_sim_map_filter_ui("sim_map_filter_1")

## To be copied in the server
# mod_sim_map_filter_server("sim_map_filter_1")
