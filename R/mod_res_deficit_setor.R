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

          # unidades espaciais
          shinyWidgets::pickerInput(ns("unidade_espacial"),
                                    label = "Divisão Espacial",
                                    multiple = FALSE,
                                    choices = list("Hexágonos" = "hexgrid",
                                                   "Setores" = "setores_sme"),
                                    # "Distritos" = "distritos"
                                    selected = "setores_sme",
                                    inline = TRUE,
                                    width = "180px"),
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
          mapdeck::mapdeckOutput(ns("map_deficit"), height = "calc(100vh - 100px)")
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

    # Data --------------------------------------------------------------------
    hex_def <- reactive({
      req(state$scenario_selected != -1)

      # mod_df <- novo_escolas_mod_vazio()
      def_df <- data.frame()

      if (DBI::dbExistsTable(state$db_con, "deficit_por_hex")) {
        def_df <- DBI::dbGetQuery(state$db_con,
                                  sprintf("SELECT * FROM deficit_por_hex WHERE id_cenario = %d",
                                          state$scenario_selected)
                                  ) |>
          tidyr::drop_na() |>
          dplyr::left_join(deficit_bfca_hex, by = c("id_hex", "ano", "faixa_idade", "etapa", "serie", "cutoff"),
                           suffix = c("_sim", "_or")) |>
          dplyr::mutate(serie = factor(serie,
                                       levels = c("creche", "pre", "anos_iniciais", "anos_finais"),
                                       labels = c("Creche", "Pré-Escola", "Fundamental I", "Fundamental II"))) |>
          dplyr::filter(serie == input$filtro_etapa)



        def_clean_df <- def_df |>
          dplyr::mutate(vagas_acessiveis_rel = vagas_acessiveis_sim - vagas_acessiveis_or,
                        deficit_rel = deficit_sim - deficit_or) |>
          dplyr::select(id_hex, cd_setor, nm_distrito, serie, ano,
                        populacao = populacao_or,
                        vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                        deficit_or, deficit_sim, deficit_rel)

      }

      return(def_clean_df)
    })

    setor_def <- reactive({
      req(hex_def())

      setor_def <- hex_def() |>
        dplyr::group_by(cd_setor, nm_distrito, serie, ano) |>
        dplyr::summarise(across(.cols = c(populacao, vagas_acessiveis_or, vagas_acessiveis_sim,
                                          deficit_or, deficit_sim), sum),
                         .groups = "drop") |>
        dplyr::mutate(vagas_acessiveis_rel = vagas_acessiveis_sim - vagas_acessiveis_or,
                      deficit_rel = deficit_sim - deficit_or)

      return(setor_def)
    })


    # Table -------------------------------------------------------------------
    footer_total <- function(values) format(sum(values, na.rm = TRUE),
                                            big.mark = ".", decimal.mark = ",")

    output$table_deficit <- reactable::renderReactable({
      req(nrow(setor_def()) > 0, state$window_height)

      sector_def_clean <- setor_def() |>
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
        resizable = TRUE,
        wrap = FALSE,
        defaultSorted = list(cd_setor = "asc", serie = "asc", ano = "asc"),

        columns = list(
          nm_distrito = reactable::colDef(name = "Distrito", filterable = TRUE, show = TRUE, minWidth = 30),
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE, show = TRUE, minWidth = 30),
          serie = reactable::colDef(name = "Etapa", filterable = TRUE, show = TRUE, minWidth = 30),

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


    # Map ---------------------------------------------------------------------
    output$map_deficit <- mapdeck::renderMapdeck({
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

    observeEvent(
      eventExpr = {
        input$unidade_espacial
        input$filtro_etapa
        input$var_mapa
      },
      handlerExpr = {
        req(hex_def(), setor_def())

        data_sf <- NULL
        if (input$unidade_espacial == "hexgrid") {
          data_sf <- dplyr::left_join(hexgrid, hex_def(), by = "id_hex") |>
            # dplyr::left_join(hexgrid_setor_lookup, by = "id_hex") |>
            dplyr::left_join(setores |> sf::st_set_geometry(NULL), by = c("cd_setor", "nm_distrito"))
        } else {
          data_sf <- dplyr::left_join(setores, setor_def())
        }

        if (input$var_mapa == "Absoluto") {
          data_sf <- dplyr::mutate(data_sf, valor = deficit_sim)
        } else {
          data_sf <- dplyr::mutate(data_sf, valor = deficit_rel)
        }

        data_sf <- dplyr::mutate(data_sf, popup = glue::glue("<div><b>DRE: </b>{nm_dre}</div> <div><b>Distrito: </b>{nm_distrito}</div> <div><b>Setor: </b>{cd_setor}</div>"))

        # create legend
        legend_converter <- function (x) as.integer(x)

        # l_palette <- "viridis"

        # if (input$mapa_variavel == "resultado") {
        l_palette <- "rdbu"
        l_values <- data_sf$valor

        max_value <- max(abs(l_values))
        l_values <- c(-max_value, l_values, max_value)
        # }

        l <- colourvalues::colour_values(
          x = l_values,
          n_summaries = 6,
          alpha = 200,
          palette = l_palette
        )

        l$colours <- l$colours[2:(length(l$colours)-1)]

        legend_title = input$var_mapa

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

        mapdeck::mapdeck_update(map_id = ns("map_deficit")) |>
          mapdeck::clear_polygon(layer_id = "base") |>
          mapdeck::add_polygon(
            data = data_sf,
            fill_colour = "color",
            fill_opacity = 200,
            highlight_colour = "#eeeeee60",
            auto_highlight = TRUE,
            layer_id = "base",
            id = "cd_setor",
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
  })
}

## To be copied in the UI
# mod_res_deficit_setor_ui("res_deficit_setor_1")

## To be copied in the server
# mod_res_deficit_setor_server("res_deficit_setor_1")
