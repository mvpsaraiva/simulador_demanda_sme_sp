#' diagnostics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_diagnostics_ui <- function(id){
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c("      350px     1fr          1fr      ",
                 "1fr   config    map_present  map_future",
                 "75px  download  map_present  map_future"
      ),
      gridlayout::grid_card(
        area = "config",
        div(
          style = "margin: 10px; padding: 10px; height: 100%; width: 100%; overflow-y: auto",
          mod_diag_config_ui("diag_config")
        )
      ),
      gridlayout::grid_card(
        area = "map_present",
        div(
          style = "margin: 10px; padding: 10px; height: 100%; width: 100%; overflow-y: auto",
          mod_diag_map_ui("diag_map_present")
        )
      ),
      gridlayout::grid_card(
        area = "map_future",
        div(
          style = "margin: 10px; padding: 10px; height: 100%; width: 100%; overflow-y: auto",
          mod_diag_map_ui("diag_map_future")
        )
      ),
      gridlayout::grid_card(
        area = "download",
        div(
          style = "margin: auto; padding: auto; height: 100%; display: flex; justify-content: center; align-items: center;",
          downloadButton(ns("btn_download_diagnostics"), "Exportar Resultados")
        )
      )
    )

  )
}

#' diagnostics Server Functions
#'
#' @noRd
mod_diagnostics_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$btn_download_diagnostics <- downloadHandler(
      filename = function() {
        xls_file <- paste0("diagnostico_demanda.xlsx")
      },

      content = function(file) {
        file.copy("data/diagnostico.xlsx", file)
      })


  })
}

## To be copied in the UI
# mod_diagnostics_ui("diagnostics_1")

## To be copied in the server
# mod_diagnostics_server("diagnostics_1")
