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
        div(
          style = "margin: 10px; padding: 10px; height: 90vh; overflow-y: auto",
          tagList(
            h4("Nenhuma escola selecionada", class = "tile-headline"),
            # h5(state$dre, class = "tile-subheadline")
            hr(),

            div(
              style = "margin: 0 10px 0 10px; padding: 0 10px 0 10px; height: 100px; float: right",
              actionButton(ns("btn_rodar_simulacao"), label = "Rodar Simulação >>>")
            )
          )
        )

      } else {
        # build ui with school details

        escola <- prepare_school_data(escolas, state$school_selected)
        deficit_setor <- prepare_sector_deficit_data(escolas, state$school_selected)
        deficit_distrito <- prepare_district_deficit_data(escolas, state$school_selected)

        div(
          style = "margin: 10px; padding: 10px; height: 90vh; overflow-y: auto",
          tagList(
            h4(paste0("Déficit por Distrito - ", escola$nm_distrito[1]), class = "tile-headline"),
            # h5(escola$nm_distrito[1], class = "tile-headline"),
            renderTable({deficit_distrito}, colnames = FALSE, width = "100%"),

            h4(paste0("Déficit por Setor - ", escola$cd_setor[1]), class = "tile-headline"),
            # h5(, class = "tile-headline"),
            renderTable({deficit_setor}, colnames = FALSE, width = "100%"),

            h4(escola$no_entidade[1], class = "tile-headline"),
            h5(paste0(escola$co_entidade[1], " | ", escola$ds_endereco[1]), class = "tile-subheadline"),
            renderTable({ escola |> dplyr::select(info, valor)}, colnames = FALSE, width = "100%"),

            actionButton(ns("btn_editar_escola"), label = "Editar Capacidade da Escola"),

            hr(),

            div(
              style = "height: 100px; float: right",
              actionButton(ns("btn_rodar_simulacao"), label = "Rodar Simulação >>>")
            )
          )
        )
      }



    })
  })
}

## To be copied in the UI
# mod_sim_school_ui("sim_school_1")

## To be copied in the server
# mod_sim_school_server("sim_school_1")
