#' estudantes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_estudantes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # Filters
      column(
        width = 3,
        wellPanel(
          radioButtons(inputId = ns("serie"),
                       label = "Faixa Etária / Série",
                       choices = list("Creche (0 a  3 anos de idade)" = "creche",
                                      "Pré-Escola (4 a  5 anos de idade)" = "pre",
                                      "Fundamental I (6 a 10 anos de idade)" = "anos_iniciais",
                                      "Fundamental II (11 a 14 anos de idade)" = "anos_finais"),
                       selected = "creche"),

          selectInput(inputId = ns("unidade_espacial"),
                      label = "Divisão Espacial",
                      choices = list("Hexágonos" = "hexgrid",
                                     "Setores" = "setores_sme",
                                     "Distritos" = "distritos"),
                      selected = "setores_sme"),
          checkboxInput(inputId = ns("mostrar_escolas"), label = "Mostrar Escolas", value = FALSE),
          selectInput(inputId = ns("previsao"),
                      label = "Previsão",
                      choices = list(2035, 2045),
                      selected = 2035)
        )
      ),
      # Maps
      column(
        width = 4,
        mapdeck::mapdeckOutput(ns("map_current"), height = "750")
      ),
      column(
        width = 4,
        mapdeck::mapdeckOutput(ns("map_future"), height = "750")
      )
    )
  )
}

#' estudantes Server Functions
#'
#' @noRd
mod_estudantes_server <- function(id, db_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    unidade_espacial <- reactive({
      input$unidade_espacial
    })

    mostrar_escolas <- reactive({
      input$mostrar_escolas
    })

    ano_previsao <- reactive({
      input$previsao
    })

    comparacao <- reactive({
      input$comparacao
    })

    spatial_sf <- reactive({
      read_sf_from_db(db_con, input$unidade_espacial)
    })

    table_df <- reactive({

      get_table_name <- function(spatial_unit) {
        # second part of table name
        p2 <- ""
        if (spatial_unit == "hexgrid") p2 <- "hex"
        if (spatial_unit == "setores_sme") p2 <- "setor"
        if (spatial_unit == "distritos") p2 <- "distrito"

        table_name = paste0("populacao_por_", p2)

        return(table_name)
      }

      table <- get_table_name(input$unidade_espacial)

      table_data <- DBI::dbReadTable(db_con, table) |>
        dplyr::filter(serie == input$serie, ano %in% c(2020, 2035, 2045))

      return(table_data)
    })

    data_joined_sf <- reactive({

      col <- ""
      if (input$unidade_espacial == "hexgrid") col <- "id_hex"
      if (input$unidade_espacial == "setores_sme") col <- "cd_setor"
      if (input$unidade_espacial == "distritos") col <- "cd_distrito"

      data_sf <- dplyr::left_join(spatial_sf(), table_df(), by = col)

      # sf::st_write(data_sf, "joined.gpkg")
      return(data_sf)
    })


    observe({

      col <- ""
      id_col <- ""
      if (unidade_espacial() == "hexgrid") {
        id_col = "id_hex"
        col <- "populacao_esc_publica_hex"
      }
      if (unidade_espacial() == "setores_sme") {
        id_col = "cd_setor"
        col <- "populacao_esc_publica_setor"
      }
      if (unidade_espacial() == "distritos") {
        id_col = "cd_distrito"
        col <- "populacao_esc_publica_distrito"
      }

      # data_filtered <- dplyr::filter(data_joined_sf(), ano %in% c(2020, ano_previsao())) |>
      #   dplyr::group_by(!!as.name(id_col)) |>
      #   dplyr::arrange(ano) |>
      #   dplyr::mutate(val = !!as.name(col)) |>
      #   dplyr::mutate(diff = dplyr::last(val) - dplyr::first(val)) |>
      #   dplyr::select(-val)

      update_map <- function(data, a, id = "_current") {

        m_id <- paste0("map", id)
        l_id <- paste0("geo", id)


        data_l <- sf::st_set_geometry(data, NULL)
        data_l <- data_l[col]
        # data_l <- na.omit(data_l)


        # ADD THE COULOURS TO THE DATA
#         fill_color <- colourvalues::colour_values(
# =          x = c(data_l[col], max(data_l[col])),
#           alpha = 200,
#           palette = "inferno"
#         )
        # print(fill_color[-length(fill_color)])
        # print(head(tempo_filtrado_sf()))
        # print(c(tempo_filtrado_sf()$valor, scale_limits()$max))

        # data <- data |>
        #   dplyr::mutate(fill = fill_color[1])
        # dplyr::mutate(fill = fill_color[-length(fill_color)])


        # create legend
        l <- colourvalues::colour_values(x = c(data_l[col],
                                               max(data_l[col], na.rm = TRUE)),
                                         alpha = 200,
                                         n_summaries = 6,
                                         palette = "inferno")

        legend_converter <- function (x) as.integer(x)

        legend <- mapdeck::legend_element(
          variables = legend_converter(l$summary_values)
          , colours = l$summary_colours
          , colour_type = "fill"
          , variable_type = "gradient"
          , title = "Estudantes de\nEscola Pública"
        )
        js_legend <- mapdeck::mapdeck_legend(legend)

        # put colors into dataframe
        # data$fill <- l$colours[1]
        data_ano <- dplyr::filter(data, ano == a)


        # f_col = col
        # if (comparacao() == "diferenca" & id == "_future") f_col = "diff"

        mapdeck::mapdeck_update(map_id = ns(m_id)) |>
          mapdeck::clear_polygon(layer_id = l_id) |>
          mapdeck::add_polygon(data = data_ano,
                               fill_opacity = 200,
                               fill_colour = col,
                               stroke_width = 2,
                               update_view = FALSE,
                               focus_layer = FALSE,
                               auto_highlight = TRUE,
                               tooltip = "popup",
                               legend = TRUE,
                               legend_options = list(title = "Estudantes de\nEscola Pública",
                                                     digits = 0)
                               # legend = js_legend
                               )

        # escolas
        if (mostrar_escolas() == TRUE) {

          escolas_df <- DBI::dbReadTable(db_con, "escolas")

          mapdeck::mapdeck_update(map_id = ns(m_id)) |>
            mapdeck::add_pointcloud(data = escolas_df,
                                    lon = "lon", lat = "lat",
                                    update_view = FALSE,
                                    layer_id = "escolas",
                                    fill_colour = "tp_categoria",
                                    legend = TRUE,
                                    fill_opacity = 170,
                                    # auto_highlight = TRUE
                                    # id = "brasil",
                                    tooltip = "no_entidade"
            )
        } else {
          mapdeck::mapdeck_update(map_id = ns(m_id)) |>
            mapdeck::clear_pointcloud(layer_id = "escolas")
        }
      }

      update_map(data_joined_sf(), 2020, "_current")
      update_map(data_joined_sf(), ano_previsao(), "_future")

      # mapdeck::mapdeck_update(map_id = ns("map_current")) |>
      #   mapdeck::clear_polygon(layer_id = "geo_current") |>
      #   mapdeck::add_polygon(data = data_joined_sf(),
      #                        fill_opacity = 200,
      #                        fill_colour = col,
      #                        stroke_width = 2
      #   )
    })

    output$map_current <- mapdeck::renderMapdeck({
      muni_sf <- read_sf_from_db(db_con, "municipio")

      mapdeck::mapdeck(location = c(-46.63306176720343, -23.548164364465265), zoom = 9) |>
        # mapdeck::mapdeck_view(location = c(-46.63306176720343, -23.548164364465265), zoom = 3,
        #              duration = 4000, transition = "fly") |>
        mapdeck::add_polygon(data = muni_sf,
                             layer_id = "geo_current")
    })

    output$map_future <- mapdeck::renderMapdeck({
      muni_sf <- read_sf_from_db(db_con, "municipio")

      mapdeck::mapdeck(location = c(-46.63306176720343, -23.548164364465265), zoom = 9) |>
        # mapdeck::mapdeck_view(location = c(-46.63306176720343, -23.548164364465265), zoom = 3,
        #              duration = 4000, transition = "fly") |>
        mapdeck::add_polygon(data = muni_sf,
                             layer_id = "geo_future")
    })

  })
}
