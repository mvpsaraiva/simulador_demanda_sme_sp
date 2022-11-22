#' res_deficit_setor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_deficit_setor_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        div(
          style = "margins: 5px",
          h4("Configurações do Mapa", class = "tile-headline"),

          # etapa de ensino
          shinyWidgets::pickerInput(ns("filtro_etapa"),
                                    label = "Etapa de ensino",
                                    multiple = FALSE,
                                    choices = c("Creche",
                                                "Pré-Escola",
                                                "Fundamental I",
                                                "Fundamental II"),
                                    selected = "Creche",
                                    inline = TRUE,
                                    width = "200px"),
          shinyWidgets::pickerInput(ns("var_mapa"),
                                    label = "Mapear Déficit/Superávit",
                                    choices = c("Absoluto", "Relativo ao atual"),
                                    selected = "Absoluto",
                                    inline = TRUE,
                                    width = "200px"),
          hr(),

          reactable::reactableOutput(ns("table_deficit"), width = "100%", height = "calc(100vh - 250px)")
        )
      ),
      column(
        width = 6,
        div(
          style = "margins: 5px",
          mapboxer::mapboxerOutput(ns("map_deficit"), height = "calc(100vh - 100px)")
        )
      )
    )
  )
}

#' res_deficit_setor Server Functions
#'
#' @noRd
mod_res_deficit_setor_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sector_def <- reactive({
      req(state$scenario_selected != -1)

      # mod_df <- novo_escolas_mod_vazio()
      def_df <- data.frame()

      if (DBI::dbExistsTable(state$db_con, "deficit_por_setor")) {
        def_df <- DBI::dbGetQuery(state$db_con,
                                  sprintf("SELECT * FROM deficit_por_setor WHERE id_cenario = %d",
                                          state$scenario_selected)
                                  ) |>
          tidyr::drop_na() |>
          dplyr::left_join(deficit_bfca_setor, by = c("cd_dre", "nr_distrito", "cd_setor",
                                                      "ano", "faixa_idade", "etapa", "serie", "cutoff"),
                           suffix = c(".sim", ".or")) |>
          dplyr::mutate(serie = factor(serie,
                                       levels = c("creche", "pre", "anos_iniciais", "anos_finais"),
                                       labels = c("Creche", "Pré-Escola", "Fundamental I", "Fundamental II"))) |>
          dplyr::filter(serie == input$filtro_etapa)



        def_df <- def_df |>
          dplyr::mutate(vagas_acessiveis_rel = vagas_acessiveis.sim - vagas_acessiveis.or,
                        deficit_rel = deficit.sim - deficit.or) |>
          dplyr::select(nm_distrito = nm_distrito.sim, cd_setor, serie, ano,
                        populacao = populacao.or,
                        vagas_acessiveis_or = vagas_acessiveis.or,
                        vagas_acessiveis_sim = vagas_acessiveis.sim,
                        vagas_acessiveis_rel,
                        deficit_or = deficit.or,
                        deficit_sim = deficit.sim,
                        deficit_rel)

      }

      return(def_df)
    })

    footer_total <- function(values) format(sum(values, na.rm = TRUE),
                                            big.mark = ".", decimal.mark = ",")

    output$table_deficit <- reactable::renderReactable({
      req(nrow(sector_def()) > 0, state$window_height)

      sector_def_clean <- sector_def() |>
        dplyr::select(nm_distrito, cd_setor, serie, ano,
                      populacao, vagas_acessiveis_or, vagas_acessiveis_sim,
                      deficit_or, deficit_sim)

      reactable::reactable(
        sector_def_clean,
        compact = TRUE,
        defaultColDef = reactable::colDef(minWidth = 30, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        defaultPageSize = round((state$window_height - 400) / 31),  # 345
        paginationType = "simple",
        searchable = FALSE,
        wrap = FALSE,
        defaultSorted = list(cd_setor = "asc", serie = "asc", ano = "asc"),

        columns = list(
          nm_distrito = reactable::colDef(name = "Distrito", filterable = TRUE, show = TRUE, minWidth = 50),
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE, show = TRUE, minWidth = 30),
          serie = reactable::colDef(name = "Etapa", filterable = TRUE, show = TRUE, minWidth = 50),

          ano = reactable::colDef(name = "Ano", filterable = TRUE, show = TRUE, minWidth = 25),

          populacao = reactable::colDef(name = "Demanda", show = TRUE, footer = footer_total),
          vagas_acessiveis_or = reactable::colDef(name = "Vagas", show = TRUE, footer = footer_total),
          vagas_acessiveis_sim = reactable::colDef(name = "Vagas (sim.)", show = TRUE, footer = footer_total),
          deficit_or = reactable::colDef(name = "Déficit", show = TRUE, footer = footer_total),
          deficit_sim = reactable::colDef(name = "Déficit (sim.)", show = TRUE, footer = footer_total)
        ),
        language = reactable::reactableLang(
          searchPlaceholder = "Pesquisar setores",
          noData = "Nenhum setor encontrado",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} setores",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}"
        )
      )

    })



# Mapa --------------------------------------------------------------------

    sector_deficit_sf <- reactive({
      req(sector_def())

      var_name <- "deficit_or"
      if (input$var_mapa == "Absoluto") {
        var_name <- "deficit_sim"
      } else {
        var_name <- "deficit_rel"
      }

      deficit_sf <-
        setores |> dplyr::left_join(sector_def(), by="cd_setor") |>
        dplyr::mutate(deficit = get(var_name))

    })


    tooltip_mb <- paste("<div><b>Distrito: </b>{{nm_distrito}}</div>",
                        "<div><b>Setor: </b>{{cd_setor}}</div>")

    tooltip_selected_mb <- "<div><b>Distrito: </b>{{nm_distrito}}</div> <div><b>Setor: </b>{{cd_setor}}</div>"
    tooltip_highlighted_mb <- "<div><b>Distrito: </b>{{nm_distrito}}</div> <div><b>Setor: </b>{{cd_setor}}</div>"

    output$map_deficit <- mapboxer::renderMapboxer({
      # req(state$scenario_selected != -1)

      state$map_id <- ns("map_deficit")
      sf_shape <- sector_deficit_sf()

      mapboxer::mapboxer(
        style = "mapbox://styles/mapbox/light-v9",
        token = golem::get_golem_options("mapbox_token"),
        center = state$centroid,
        zoom = 10,
        minZoom = 8
        )  |>

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
          fill_color = "rgba(64,64,64,0.1)", fill_outline_color = "rgba(64,64,64,0.5)", id = "base",
          fill_sort_key = 1, source = mapboxer::as_mapbox_source(sf_shape)
        ) |>

        # map overlay of all meshblocks
        mapboxer::add_fill_layer(
          fill_color = c("get", "color"), fill_outline_color = "rgba(64,64,64,0.5)", id = "mb",
          fill_opacity = 0.5,
          fill_sort_key = 1, source = mapboxer::as_mapbox_source(setores[0,])
        ) |>

        # add tooltips for all layers
        mapboxer::add_tooltips(layer_id = "mb", tooltip = tooltip_mb) |>
        mapboxer::add_tooltips(layer_id = "highlight-mb", tooltip = tooltip_highlighted_mb) |>
        # add_tooltips(layer_id = "highlight-bucket", tooltip = tooltip_highlighted_bucket) |>
        mapboxer::add_tooltips(layer_id = "selected-mb", tooltip = tooltip_selected_mb) |>

        mapboxer::fit_bounds(sf::st_bbox(sf_shape),
                             options = list(options.easing = list(animate = FALSE))
                             )
    })




  })
}

## To be copied in the UI
# mod_res_deficit_setor_ui("res_deficit_setor_1")

## To be copied in the server
# mod_res_deficit_setor_server("res_deficit_setor_1")
