#' sim_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    mapboxer::mapboxerOutput(ns("map"), height = "700")
    # uiOutput(ns("selected_area"))
  )
}

#' sim_map Server Functions
#'
#' @noRd
mod_sim_map_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Shape data --------------------------------------------------------------

    observeEvent(state$dre, {
      req(map_rendered())

      # filter shapefile by selected region
      if (state$dre == app_states$INITIAL_DRE) {
        sf_shape <- setores
      } else {
        sf_shape <- setores |> dplyr::filter(nm_dre == state$dre)
      }

      mapboxer::mapboxer_proxy(ns("map")) |>
        mapboxer::set_data(data = sf_shape, source_id = "mb") |>
        mapboxer::fit_bounds(sf::st_bbox(sf_shape)) |>
        mapboxer::update_mapboxer()
    })

    # Map ---------------------------------------------------------------------

    tooltip_mb <- "<div>{{cd_setor}}</div>"
    tooltip_selected_mb <- "<div><div style='font-weight: bold'>{{SA22018__1}}</div><div style='white-space:nowrap'>{{commute_all}} residents work in <strong>{{name}}</strong></div></div>"
    tooltip_highlighted_mb <- "<div><div style='font-size: 16px'><strong>{{commute_all}}</strong> commuters</div><div>{{direction}} <strong>{{SA22018__1}}</strong></div></div>"

    map_rendered <- reactiveVal(FALSE)
    output$map <- mapboxer::renderMapboxer({
      sf_shape <- setores # setores # SF_SHAPE |> filter(regc2020_name == INITIAL_REGION)
      state$map_id <- ns("map")
      map_rendered(TRUE)

      mapboxer::mapboxer(style = "mapbox://styles/mapbox/dark-v9", token = golem::get_golem_options("mapbox_token"))  |>

        # meshblock highlights
        mapboxer::add_fill_layer(
          fill_color = c("get", "color"), fill_opacity = 0.5, id = "highlight-mb",
          source = mapboxer::as_mapbox_source(setores[0,] |> dplyr::mutate(color = "#000000")),
          fill_sort_key = 10
        ) |>

        # map overlay showing the clicked-on meshblock
        mapboxer::add_fill_layer(
          fill_color = theme$COLOR_ORANGE,
          fill_opacity = 0.75, id = "selected-mb",
          source = mapboxer::as_mapbox_source(setores[0,]), fill_sort_key = 12
        ) |>

        # map overlay of all meshblocks
        mapboxer::add_fill_layer(
          fill_color = "rgba(255,255,255,0.1)", fill_outline_color = "rgba(255,255,255,0.5)", id = "mb",
          fill_sort_key = 1, source = mapboxer::as_mapbox_source(sf_shape)
        ) |>

        # add tooltips for all layers
        mapboxer::add_tooltips(layer_id = "mb", tooltip = tooltip_mb) |>
        mapboxer::add_tooltips(layer_id = "highlight-mb", tooltip = tooltip_highlighted_mb) |>
        # add_tooltips(layer_id = "highlight-bucket", tooltip = tooltip_highlighted_bucket) |>
        mapboxer::add_tooltips(layer_id = "selected-mb", tooltip = tooltip_selected_mb) |>

        mapboxer::fit_bounds(sf::st_bbox(sf_shape))
    })

    # Map clicks --------------------------------------------------------------

    observeEvent(input$map_onclick, {
      # unpack the meshblock associated with the selected shape
      selected_mb <- input$map_onclick$props$cd_setor

      # change the app state
      state$state <- list(
        id = STATE_MB_SELECTED,
        store = list(selected_mb = selected_mb, event_source = "map")
      )
    })

    # Highlight map -----------------------------------------------------------

    # highlighted meshblocks after map click
    d_highlighted_mb <- reactive({
      req(state$state$id == STATE_MB_SELECTED)

      # obter distrito do setor selecionado
      cd_dist <- setores |>
        sf::st_set_geometry(NULL) |>
        dplyr::filter(cd_setor == state$state$store$selected_mb) |>
        dplyr::select(nr_distrito) |>
        dplyr::pull()

      # retornar setores no mesmo distrito
      setores |>
        dplyr::filter(nr_distrito == cd_dist) |>
        dplyr::mutate(selected_mb = state$state$store$selected_mb)
    })

    # redraw map on map click
    observeEvent(d_highlighted_mb(), {
      # only highlight map if app is in STATE_MB_SELECTED
      req(state$state$id == STATE_MB_SELECTED)

      # extract highlighted shapes
      # bbox <- sf::st_bbox(d_highlighted_mb())

      sf_selected_shape <- d_highlighted_mb() |> dplyr::filter(cd_setor == selected_mb)
      sf_district_shapes <- d_highlighted_mb() |> dplyr::filter(cd_setor != selected_mb)


      # push new data into the `highlight-mb` map layer
      # NOTE: we need to use `ns()` to retrieve the correct map object
      mapboxer::mapboxer_proxy(ns("map")) |>
        mapboxer::set_data(data = sf_district_shapes, source_id = "highlight-mb") |>
        mapboxer::set_data(data = sf_selected_shape, source_id = "selected-mb") |>

        # mapboxer::fit_bounds(bbox, maxZoom = 10) |>
        mapboxer::update_mapboxer()
    })

    # delete all map highlight if the app state changes to STATE_NOTHING_SELECTED
    observeEvent(state$state, {
      req(state$state$id == STATE_NOTHING_SELECTED)
      mapboxer::mapboxer_proxy(ns("map")) |>
        mapboxer::set_data(data = setores[0,], source_id = "highlight-mb") |>
        mapboxer::set_data(data = setores[0,], source_id = "selected-mb") |>
        mapboxer::update_mapboxer()
    })





  })
}

## To be copied in the UI
# mod_sim_map_ui("sim_map_1")

## To be copied in the server
# mod_sim_map_server("sim_map_1")
