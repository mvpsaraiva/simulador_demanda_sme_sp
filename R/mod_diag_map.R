#' diag_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_diag_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("map_title")),
    mapdeck::mapdeckOutput(ns("map"), height = "90vh")
  )
}

#' diag_map Server Functions
#'
#' @noRd
mod_diag_map_server <- function(id, state, indice_ano){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Title
    output$map_title <- renderUI({
      req(state$config$anos)

      titulo = paste("Mapa -", state$config$anos[indice_ano])
      h4(titulo, class = "tile-headline")
    })

    # Map ---------------------------------------------------------------------
    output$map <- mapdeck::renderMapdeck({
      sf_shape <- setores |>
        dplyr::mutate(popup = glue::glue("<div><b>DRE: </b>{nm_dre}</div> <div><b>Distrito: </b>{nm_distrito}</div> <div><b>Setor: </b>{cd_setor}</div>"))

      state$map_id <- ns("map")

      mapdeck::mapdeck(
        style = "mapbox://styles/mapbox/light-v9",
        location = state$centroid,
        zoom = 9,
        min_zoom = 8
      ) |>
        mapdeck::add_polygon(
          data = sf_shape,
          # polyline = "geometry",
          fill_colour = "#bfbfbf60",
          fill_opacity = 255,
          highlight_colour = "#eeeeee60",
          auto_highlight = TRUE,
          layer_id = "base",
          id = "cd_setor",
          update_view = FALSE,
          focus_layer = FALSE,
          tooltip = "popup",
          stroke_width = 15,
          stroke_colour = "#404040ff",
          stroke_opacity = 255
        )
    })

    map_shape = reactive({
      return(state$config$map_shape)
    })

    map_data = reactive({
      req(state$config$map_data)

      # browser()

      if (is.data.frame(state$config$map_data)) {

        if (state$config$mapa_variavel == "oferta") {
          return(state$config$map_data)
        } else {
          return(
            state$config$map_data |>
              dplyr::filter(ano == state$config$anos[indice_ano])
          )
        }
      }
      return(state$config$map_data)
    })

    observeEvent(
      eventExpr = {
        map_shape()
        map_data()
      },
      handlerExpr = {
        req(map_shape(), map_data())


        if (state$config$unidade_espacial == "nenhuma") {
          mapdeck::mapdeck_update(map_id = ns("map")) |>
            mapdeck::clear_polygon(layer_id = "base")

          return(NULL)
        }

        data_sf <- dplyr::left_join(map_shape(), map_data())

        # create legend
        legend_converter <- function (x) as.integer(x)

        l_palette <- "viridis"
        l_values <- c(data_sf$valor)

        if (state$config$mapa_variavel == "deficit") {
          l_palette <- "reds"

          # max_value <- max(abs(l_values))
          # l_values <- c(-max_value, l_values, max_value)
        }
        if (state$config$mapa_variavel == "superavit") {
          l_palette <- "blues"

          # max_value <- max(abs(l_values))
          # l_values <- c(-max_value, l_values, max_value)
        }

        l <- colourvalues::colour_values(
          x = l_values,
          n_summaries = 6,
          alpha = 200,
          palette = l_palette
        )

        legend_title = switch (
          state$config$mapa_variavel,
          "demanda" = "Demanda (estudantes)",
          "oferta" = "Oferta (vagas)",
          "deficit" = "Déficit de vagas",
          "superavit" = "Superávit de vagas"
        )

        legend <- mapdeck::legend_element(
          variables = legend_converter(l$summary_values)
          , colours = l$summary_colours
          , colour_type = "fill"
          , variable_type = "gradient"
          , title = legend_title
        )
        js_legend <- mapdeck::mapdeck_legend(legend)


        data_sf$color <- l$colours
        data_sf <- data_sf |>
          dplyr::mutate(popup = glue::glue("{popup} <div><b>{legend_title}: </b>{valor}</div>"))

        mapdeck::mapdeck_update(map_id = ns("map")) |>
          mapdeck::clear_polygon(layer_id = "base") |>
          mapdeck::add_polygon(
            data = data_sf,
            fill_colour = "color",
            fill_opacity = 200,
            highlight_colour = "#eeeeee60",
            auto_highlight = TRUE,
            layer_id = "base",
            id = "id_hex",
            update_view = FALSE,
            focus_layer = FALSE,
            tooltip = "popup",

            legend = js_legend,

            stroke_width = 15,
            stroke_colour = "#404040ff",
            stroke_opacity = 255
          )
      }
    )

    mapa_exibir_escolas = reactive({
      return(state$config$mapa_exibir_escolas)
    })

    school_data = reactive({
      return(state$config$schools)
    })

    observeEvent(
      eventExpr = {
        mapa_exibir_escolas()
        school_data()
      },
      handlerExpr = {
        mapdeck::mapdeck_update(map_id = ns("map")) |>
          mapdeck::clear_pointcloud(layer_id = "schools")

        if (nrow(school_data()) > 0) {

          legend <- mapdeck::legend_element(
            variables = c("Conveniada", "Municipal", "Estadual"),
            colours = c("#415a77FF", "#ffb703FF", "#c1121fFF"),
            colour_type = "fill",
            variable_type = "category",
            title = "Escolas"
          )
          js_legend <- mapdeck::mapdeck_legend(legend)

          mapdeck::mapdeck_update(map_id = ns("map")) |>
            mapdeck::add_pointcloud(
              data = school_data(),
              lat = "lat",
              lon = "lon",
              radius = 8,
              fill_colour = "color",
              fill_opacity = 255,
              highlight_colour = "#eeeeee60",
              auto_highlight = TRUE,
              layer_id = "schools",
              id = "co_entidade",
              update_view = FALSE,
              focus_layer = FALSE,
              tooltip = "popup",
              palette = NULL,

              legend = js_legend
            )
        }

      }
    )

    mapa_exibir_metro = reactive({
      return(state$config$mapa_exibir_metro)
    })

    metro_data = reactive({
      return(state$config$metro)
    })

    observeEvent(
      eventExpr = {
        mapa_exibir_metro()
        metro_data()
      },
      handlerExpr = {

        mapdeck::mapdeck_update(map_id = ns("map")) |>
          mapdeck::clear_pointcloud(layer_id = "metro")

        if (nrow(metro_data()) > 0) {

          mapdeck::mapdeck_update(map_id = ns("map")) |>
            mapdeck::add_pointcloud(
              data = metro_data(),
              lat = "lat",
              lon = "lon",
              radius = 8,
              fill_colour = "modo",
              fill_opacity = 255,
              highlight_colour = "#eeeeee60",
              auto_highlight = TRUE,
              layer_id = "metro",
              update_view = FALSE,
              focus_layer = FALSE,
              tooltip = "popup",
              palette = "ygobb",
              legend = TRUE,
              legend_options = list(title = "Transporte")
            )
        }

      }
    )



  })
}

## To be copied in the UI
# mod_diag_map_ui("diag_map_1")

## To be copied in the server
# mod_diag_map_server("diag_map_1")
