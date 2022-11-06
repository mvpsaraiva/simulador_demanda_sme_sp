#' visualizacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visualizacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # Filters
      column(
        width = 3,
        wellPanel(
          selectInput(inputId = ns("variavel"),
                      label = "Mapear",
                      choices = list("População em idade escolar" = "populacao",
                                     "Vagas em escolas públicas" = "vagas"),
                      selected = "populacao"),
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
                      selected = "setores_sme")
        )
      ),
      # Maps
      column(
        width = 4,
        mapdeck::mapdeckOutput(ns("map_current"), height = "600")
      ),
      column(
        width = 4,
        mapdeck::mapdeckOutput(ns("map_future"), height = "600")
      ),
    )
  )
}

#' visualizacao Server Functions
#'
#' @noRd
mod_visualizacao_server <- function(id, db_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    unidade_espacial <- reactive({
      input$unidade_espacial
    })

    spatial_sf <- reactive({
      read_sf_from_db(db_con, input$unidade_espacial)
    })

    table_df <- reactive({

      get_table_name <- function(var, spatial_unit) {
        # first part of table name
        p1 <- ""
        if (var == "populacao") p1 <- "populacao"
        if (var == "vagas") p1 <- "matriculas"

        # second part of table name
        p2 <- ""
        if (spatial_unit == "hexgrid") p2 <- "hex"
        if (spatial_unit == "setores_sme") p2 <- "setor"
        if (spatial_unit == "distritos") p2 <- "distrito"

        table_name = paste0(p1, "_por_", p2)

        return(table_name)
      }

      table <- get_table_name(input$variavel, input$unidade_espacial)

      table_data <- DBI::dbReadTable(db_con, table) |>
        dplyr::filter(serie == input$serie)

      if (input$variavel == "populacao")
        table_data <- dplyr::filter(table_data, ano %in% c(2020, 2035, 2045))

      return(table_data)
    })


    data_joined_sf <- reactive({

      col <- ""
      if (input$unidade_espacial == "hexgrid") col <- "id_hex"
      if (input$unidade_espacial == "setores_sme") col <- "cd_setor"
      if (input$unidade_espacial == "distritos") col <- "cd_distrito"

      data_sf <- dplyr::left_join(spatial_sf(), table_df(), by = col)

      return(data_sf)
    })


    observe({

      col <- ""
      if (input$unidade_espacial == "hexgrid") col <- " populacao_serie_hex"
      if (input$unidade_espacial == "setores_sme") col <- "populacao_serie_setor"
      if (input$unidade_espacial == "distritos") col <- "populacao_serie_distrito"


      mapdeck::mapdeck_update(map_id = ns("map_current")) |>
          mapdeck::clear_polygon(layer_id = "geo_current") |>
          mapdeck::add_polygon(data = data_joined_sf(),
                               fill_opacity = 200,
                               fill_colour = col,
                               stroke_width = 2
                               )
    })

    output$map_current <- mapdeck::renderMapdeck({
      # spatial_sf <- read_sf_from_db(db_con, input$unidade_espacial)
      muni_sf <- read_sf_from_db(db_con, "municipio")

      mapdeck::mapdeck() |>
        mapdeck::add_polygon(data = muni_sf,
                             layer_id = "geo_current")
    })

    output$map_future <- mapdeck::renderMapdeck({
      # spatial_sf <- read_sf_from_db(db_con, input$unidade_espacial)
      muni_sf <- read_sf_from_db(db_con, "municipio")

      mapdeck::mapdeck() |>
        mapdeck::add_polygon(data = muni_sf,
                             layer_id = "geo_future")
    })

  })
}

## To be copied in the UI
# mod_visualizacao_ui("visualizacao_1")

## To be copied in the server
# mod_visualizacao_server("visualizacao_1")
