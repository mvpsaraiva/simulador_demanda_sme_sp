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

    tabsetPanel(
      id = ns("tab_sim_map"),
      tabPanel(title = "Mapa",
               mapdeck::mapdeckOutput(ns("map"), height = "100%")
      ),
      tabPanel(title = "Configurações",
               style = "margin: 5px; padding: 5px;",
               div(
                 style = "margins: 5px; padding: 5px;",
                 h4("Filtro", class = "tile-headline"),

                 # DRE
                 shinyWidgets::pickerInput(ns("filtro_dre"),
                                           label = "DREs",
                                           multiple = TRUE,
                                           choices = NULL,
                                           inline = TRUE,
                                           width = "200px"),

                 # Distrito
                 shinyWidgets::pickerInput(ns("filtro_distrito"),
                                           label = "Distritos",
                                           multiple = TRUE,
                                           choices = NULL,
                                           inline = TRUE,
                                           width = "300px"),

                 h4("Unidade Espacial", class = "tile-headline"),

                 # unidades espaciais
                 shinyWidgets::pickerInput(ns("unidade_espacial"),
                             label = "Divisão Espacial",
                             multiple = FALSE,
                             choices = list("Hexágonos" = "hexgrid",
                                            "Setores" = "setores_sme"),
                                            # "Distritos" = "distritos"
                             selected = "setores_sme",
                             inline = TRUE,
                             width = "200px"),

                 h4("Configurações do Mapa", class = "tile-headline"),

                 # variável
                 shinyWidgets::pickerInput(ns("mapa_variavel"),
                                           label = "Mapear",
                                           multiple = FALSE,
                                           choices = c("Demanda (estudantes)" = "demanda",
                                                       "Oferta (vagas)" = "oferta",
                                                       "Resultado (déficit ou superávit)" = "resultado"),
                                           selected = "demanda",
                                           inline = TRUE,
                                           width = "500px"),
                 # etapa de ensino
                 shinyWidgets::pickerInput(ns("mapa_etapa"),
                                           label = "Etapa de ensino",
                                           multiple = TRUE,
                                           choices = c("Creche" = "creche",
                                                       "Pré-Escola" = "pre",
                                                       "Fundamental I" = "anos_iniciais",
                                                       "Fundamental II" = "anos_finais"),
                                           selected = c("creche", "pre", "anos_iniciais", "anos_finais"),
                                           inline = TRUE,
                                           width = "500px"),
                 # rede de ensino
                 shinyWidgets::pickerInput(ns("mapa_rede"),
                                           label = "Rede de ensino",
                                           multiple = FALSE,
                                           choices = c("Pública" = "publica",
                                                       "Municipal" = "municipal",
                                                       "Estadual" = "estadual"),
                                           selected = "publica",
                                           inline = TRUE,
                                           width = "200px"),
                 # etapa de ensino
                 shinyWidgets::pickerInput(ns("mapa_ano"),
                                           label = "Ano da previsão",
                                           multiple = FALSE,
                                           choices = c("2020" = 2020,
                                                       "2035" = 2035,
                                                       "2045" = 2045),
                                           selected = "2020",
                                           inline = TRUE,
                                           width = "200px")
               )
      )

    )
  )

}

#' sim_map Server Functions
#'
#' @noRd
mod_sim_map_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # DRE e Distritos para filtro
    observe({
      req(setores)

      # DRE
      dre_list <- setores$nm_dre |>
        unique() |>
        sort()
      shinyWidgets::updatePickerInput(session, "filtro_dre", choices = dre_list, selected = "")

      # Distritos
      district_list <- setores$nm_distrito |>
        unique() |>
        sort()
      shinyWidgets::updatePickerInput(session, "filtro_distrito", choices = district_list, selected = "")
    })

    observeEvent(input$filtro_dre, {
      if (length(input$filtro_dre) > 0) {
        state$selected_dres = input$filtro_dre

        shinyWidgets::updatePickerInput(session, "filtro_distrito", selected = "")
        state$selected_districts = NULL
      } else {
        state$selected_dres = NULL
      }
    })

    observeEvent(input$filtro_distrito, {
      if (length(input$filtro_distrito) > 0) {
        state$selected_districts = input$filtro_distrito

        shinyWidgets::updatePickerInput(session, "filtro_dre", selected = "")
        state$selected_dres = NULL
      } else {
        state$selected_districts = NULL
      }
    })

    observeEvent(input$mapa_variavel, {
      if (input$mapa_variavel == "demanda") {
        # demanda = população de estudantes
        # remover opção de rede de ensino
        shinyWidgets::updatePickerInput(session, "mapa_rede", selected = "")
        if (length(input$mapa_ano) == 0) {
          shinyWidgets::updatePickerInput(session, "mapa_ano", selected = 2020)
        }
      }

      if (input$mapa_variavel == "oferta") {
        # oferta = número de vagas (matriculas)
        # remover opção de ano
        shinyWidgets::updatePickerInput(session, "mapa_ano", selected = "")
        if (length(input$mapa_rede) == 0) {
          shinyWidgets::updatePickerInput(session, "mapa_rede", selected = "publica")
        }
      }

      if (input$mapa_variavel == "resultado") {
        # resultado = déficit de vagas, calculado via bfca
        # remover opção de selecionar multiplas etapas
        if (length(input$mapa_etapa) > 1) {
          shinyWidgets::updatePickerInput(session, "mapa_etapa",
                                          selected = input$mapa_etapa[1])
        }
      }

    })

    observeEvent(input$mapa_etapa, {
      if ((input$mapa_variavel == "resultado") & length(input$mapa_etapa) > 1) {
        showModal(modalDialog("Apenas uma etapa de ensino pode ser selecionada por vez para a visualização de déficit/superávit de vagas."))

        shinyWidgets::updatePickerInput(session, "mapa_etapa",
                                        selected = last(input$mapa_etapa))
      }
    })

    # Shape data --------------------------------------------------------------
    map_shape <- reactive({
      sf_shape <- NULL
      if (input$unidade_espacial == "hexgrid") {
        sf_shape <- dplyr::left_join(hexgrid, hexgrid_setor_lookup, by = "id_hex") |>
          dplyr::left_join(setores |> sf::st_set_geometry(NULL), by = "cd_setor")
      } else {
        sf_shape <- setores
      }

      sf_shape <- sf_shape |>
        dplyr::mutate(popup = glue::glue("<div><b>DRE: </b>{nm_dre}</div> <div><b>Distrito: </b>{nm_distrito}</div> <div><b>Setor: </b>{cd_setor}</div>"))

      if (length(input$filtro_dre) == 0 & length(input$filtro_distrito) == 0) {
        return(sf_shape)
      } else {
        if (length(input$filtro_dre) > 0) {
          return(dplyr::filter(sf_shape, nm_dre %in% input$filtro_dre))
        } else {
          return(dplyr::filter(sf_shape, nm_distrito %in% input$filtro_distrito))
        }
      }
    })

    map_data <- reactive({
      if (input$mapa_variavel == "demanda") {
        demanda <- populacao_por_hex |>
          dplyr::filter(serie %in% input$mapa_etapa, ano == req(input$mapa_ano)) |>
          dplyr::group_by(id_hex, ano) |>
          dplyr::summarise(valor = sum(populacao_serie_hex), .groups = "drop")

        if (input$unidade_espacial == "setores_sme") {
          demanda <- dplyr::left_join(demanda, hexgrid_setor_lookup, by = "id_hex") |>
            dplyr::group_by(cd_setor, ano) |>
            dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
        }

        return(demanda)
      }

      if (input$mapa_variavel == "oferta") {
        oferta <- matriculas_por_hex |>
          dplyr::filter(serie %in% input$mapa_etapa, rede == req(input$mapa_rede)) |>
          dplyr::group_by(id_hex) |>
          dplyr::summarise(valor = sum(matriculas), .groups = "drop")

        if (input$unidade_espacial == "setores_sme") {
          oferta <- dplyr::left_join(oferta, hexgrid_setor_lookup, by = "id_hex") |>
            dplyr::group_by(cd_setor) |>
            dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
        }

        return(oferta)
      }

      if (input$mapa_variavel == "resultado") {
        resultado <- deficit_bfca_hex |>
          dplyr::filter(serie %in% input$mapa_etapa, ano == req(input$mapa_ano)) |>
          dplyr::group_by(id_hex) |>
          dplyr::summarise(valor = sum(deficit), .groups = "drop")

        if (input$unidade_espacial == "setores_sme") {
          resultado <- dplyr::left_join(resultado, hexgrid_setor_lookup, by = "id_hex") |>
            dplyr::group_by(cd_setor) |>
            dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
        }

        return(resultado)
      }

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

    observeEvent(
      eventExpr = {
        map_shape()
        map_data()
      },
      handlerExpr = {
        req(map_shape(), map_data())

        data_sf <- dplyr::left_join(map_shape(), map_data())

        # create legend
        legend_converter <- function (x) as.integer(x)

        l_palette <- "viridis"
        l_values <- c(data_sf$valor)

        if (input$mapa_variavel == "resultado") {
          l_palette <- "rdbu"

          max_value <- max(abs(l_values))
          l_values <- c(-max_value, l_values, max_value)
        }

        l <- colourvalues::colour_values(
          x = l_values,
          n_summaries = 6,
          alpha = 200,
          palette = l_palette
        )


        if (input$mapa_variavel == "resultado") {
          l$colours <- l$colours[2:(length(l$colours)-1)]
        }

        legend_title = switch (
          input$mapa_variavel,
          "demanda" = "Demanda (estudantes)",
          "oferta" = "Oferta (vagas)",
          "resultado" = "Resultado (déficit ou superávit)"
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
