#' sim_add_school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_add_school_ui <- function(id){
  ns <- NS(id)
  tagList(

    gridlayout::grid_container(
      layout = c("       400px ",
                 "600px  data  "
      ),
      gridlayout::grid_card(
        "data",
        h4("Adicionar Escola", class = "tile-headline"),
        # textInput(inputId = ns("co_entidade"),
        #           label = "Código",
        #           value = "",
        #           placeholder = "0",
        #           width = "100%"),
        textInput(inputId = ns("no_entidade"),
                  label = "Nome",
                  placeholder = "Nome da Escola",
                  width = "100%"),
        # textInput(inputId = ns("cd_setor"),
        #           label = "Setor",
        #           placeholder = "Setor",
        #           width="100%"),
        # textInput(inputId = ns("nm_distrito"),
        #           label = "Distrito",
        #           placeholder = "Distrito",
        #           width="100%"),
        # textInput(inputId = ns("nm_dre"),
        #           label = "DRE",
        #           placeholder = "DRE",
        #           width="100%"),
        numericInput(inputId = ns("lat"),
                     label = "Latitude",
                     value = -23.64986,
                     width="100%"),
        numericInput(inputId = ns("lon"),
                     label = "Longitude",
                     value = -46.64806,
                     width="100%"),
        numericInput(inputId = ns("qt_mat_inf_cre"),
                     label = "Vagas de Creche",
                     value = 0,
                     width="100%"),
        numericInput(inputId = ns("qt_mat_inf_cre"),
                     label = "Vagas de Creche",
                     value = 0,
                     width="100%"),
      )
    )

  )
}

#' sim_add_school Server Functions
#'
#' @noRd
mod_sim_add_school_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles()
      })

    output$map <- leaflet::renderLeaflet({
      state$map_id <- ns("map")

      leaflet::leaflet(
        # options = leaflet
        style = "mapbox://styles/mapbox/light-v9",
        location = state$centroid,
        zoom = 9,
        min_zoom = 8
      )

    })


    observe({
      click = input$map_click
      mapdeck::mapdeck_update(map_id = ns("map")) |>
        mapdeck::clear_pointcloud(layer_id = "new_school") |>
        mapdeck::add_pointcloud(
            data = data.frame(lon = click$lng, lat = click$lat),
            lat = "lat",
            lon = "lon",
            fill_colour = "red",
            fill_opacity = 255,
            highlight_colour = "orange",
            auto_highlight = TRUE,
            layer_id = "new_school",
            id = "co_entidade",
            update_view = FALSE,
            focus_layer = FALSE,
            tooltip = "popup",
            stroke_width = 15,
            stroke_colour = "#404040ff",
            stroke_opacity = 255
          )
      # map

      # leaflet::leafletProxy('map') |>
      #   leaflet::addMarkers(lng = click$lng, lat = click$lat)
    }) |> bindEvent(input$map_click)

  })
}

## To be copied in the UI
# mod_sim_add_school_ui("sim_add_school_1")

## To be copied in the server
# mod_sim_add_school_server("sim_add_school_1")
