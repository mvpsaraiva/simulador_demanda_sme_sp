#' res_summary_area UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_summary_area_ui <- function(id){
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
                                    multiple = TRUE,
                                    choices = c("Creche",
                                                "Pré-Escola",
                                                "Fundamental I",
                                                "Fundamental II"),
                                    selected = "Creche",
                                    inline = TRUE,
                                    width = "180px"),
          shinyWidgets::pickerInput(ns("var_mapa"),
                                    label = "Mapear vagas",
                                    choices = c("Vagas Atuais", "Vagas Simuladas", "Diferença"),
                                    selected = "Diferença",
                                    inline = TRUE,
                                    width = "180px"),
          reactable::reactableOutput(ns("table_mod"), width = "100%", height = "calc(100vh - 100px)")
        )
      ),
      column(
        width = 6,
        div(
          style = "margins: 5px",
          mapdeck::mapdeckOutput(ns("map_mod"), height = "calc(100vh - 100px)")
        )
      )
    )
  )
}

#' res_summary_area Server Functions
#'
#' @noRd
mod_res_summary_area_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    school_mod <- reactive({
      req(state$scenario_selected != -1)

      # browser()
      mod_df <- novo_escolas_mod_vazio()

      if (DBI::dbExistsTable(state$db_con, "modificacoes")) {
        mod_df <- DBI::dbGetQuery(state$db_con,
                                  sprintf("SELECT * FROM modificacoes WHERE id_cenario = %d",
                                          state$scenario_selected)
        )

        mod_df <- dplyr::left_join(mod_df, escolas, by = c("co_entidade", "no_entidade")) |>
          dplyr::select(nm_dre, nm_distrito, cd_setor, id_hex, co_entidade, no_entidade, tp_categoria, tmi_metro,
                        orig_mat_creche, nova_mat_creche, orig_mat_pre,nova_mat_pre,
                        orig_mat_fund_ai, nova_mat_fund_ai, orig_mat_fund_af, nova_mat_fund_af) |>
          dplyr::arrange(nm_dre, nm_distrito, cd_setor, no_entidade)
      }

      return(mod_df)
    })

    hex_mod <- reactive({
      req(school_mod())

      # browser()

      # escolas_full <- escolas |>
      #   dplyr::mutate(nova_mat_creche = qt_mat_inf_cre,
      #                 nova_mat_pre = qt_mat_inf_pre,
      #                 nova_mat_fund_ai = qt_mat_fund_ai,
      #                 nova_mat_fund_af = qt_mat_fund_af) |>
      #   dplyr::select(nm_dre, nm_distrito, cd_setor, id_hex,
      #                 co_entidade, no_entidade, tp_categoria, tmi_metro,
      #                 orig_mat_creche = qt_mat_inf_cre,
      #                 nova_mat_creche,
      #                 orig_mat_pre = qt_mat_inf_pre,
      #                 nova_mat_pre,
      #                 orig_mat_fund_ai = qt_mat_fund_ai,
      #                 nova_mat_fund_ai,
      #                 orig_mat_fund_af = qt_mat_fund_af,
      #                 nova_mat_fund_af) |>
      #     dplyr::filter(!(co_entidade %in% school_mod()$co_entidade)) |>
      #   dplyr::left_join(school_mod())

      mod_hex <- data.frame(
        nm_dre = character(),
        nm_distrito = character(),
        cd_setor = character(),
        id_hex = character(),
        etapa = character(),
        orig = numeric(),
        nova = numeric(),
        dif = numeric()
      )

      if (nrow(school_mod()) > 0 ) {
        mod_df <- school_mod() |>
          dplyr::group_by(nm_dre, nm_distrito, cd_setor, id_hex) |>
          dplyr::summarise(dplyr::across(.cols = c(orig_mat_creche, nova_mat_creche,
                                                   orig_mat_pre,nova_mat_pre,
                                                   orig_mat_fund_ai, nova_mat_fund_ai,
                                                   orig_mat_fund_af, nova_mat_fund_af),
                                         .fns = sum, na.rm = TRUE),
                           .groups = "drop")

        mod_hex <- mod_df |>
          tidyr::pivot_longer(cols = orig_mat_creche:nova_mat_fund_af) |>
          tidyr::separate(col = name, into = c("cenario", "mat", "etapa"), extra = "merge") |>
          tidyr::pivot_wider(names_from = cenario, values_from = value) |>
          dplyr::select(-mat) |>
          dplyr::mutate(dif = nova - orig)

        mod_hex$etapa <- factor(mod_hex$etapa,
                                  levels = c("creche", "pre", "fund_ai", "fund_af"),
                                  labels = c("Creche", "Pré-Escola", "Fund. I", "Fund. II"))
      }

      return(mod_hex)
    })

    sector_mod <- reactive({
      # req(sector_mod())

      mod_df <- hex_mod() |>
        dplyr::group_by(nm_dre, nm_distrito, cd_setor, etapa) |>
        dplyr::summarise(dplyr::across(.cols = c(orig, nova, dif),
                                       .fns = sum, na.rm = TRUE),
                         .groups = "drop")

      return(mod_df)
    })

    # sector_mod <- reactive({
    #   req(school_mod())
    #
    #   mod_setor <- data.frame(
    #     nm_dre = character(),
    #     nm_distrito = character(),
    #     cd_setor = character(),
    #     etapa = character(),
    #     orig = numeric(),
    #     nova = numeric(),
    #     dif = numeric()
    #   )
    #
    #   if (nrow(school_mod()) > 0 ) {
    #     mod_df <- school_mod() |>
    #       dplyr::group_by(nm_dre, nm_distrito, cd_setor) |>
    #       dplyr::summarise(dplyr::across(.cols = c(orig_mat_creche, nova_mat_creche,
    #                                                orig_mat_pre,nova_mat_pre,
    #                                                orig_mat_fund_ai, nova_mat_fund_ai,
    #                                                orig_mat_fund_af, nova_mat_fund_af),
    #                                      .fns = sum, na.rm = TRUE),
    #                        .groups = "drop")
    #
    #     mod_setor <- mod_df |>
    #       tidyr::pivot_longer(cols = orig_mat_creche:nova_mat_fund_af) |>
    #       tidyr::separate(col = name, into = c("cenario", "mat", "etapa"), extra = "merge") |>
    #       tidyr::pivot_wider(names_from = cenario, values_from = value) |>
    #       dplyr::select(-mat) |>
    #       dplyr::mutate(dif = nova - orig)
    #
    #     mod_setor$etapa <- factor(mod_setor$etapa,
    #                               levels = c("creche", "pre", "fund_ai", "fund_af"),
    #                               labels = c("Creche", "Pré-Escola", "Fund. I", "Fund. II"))
    #   }
    #
    #   return(mod_setor)
    # })

    district_mod <- reactive({
      # req(sector_mod())

      mod_df <- sector_mod() |>
        dplyr::group_by(nm_dre, nm_distrito, etapa) |>
        dplyr::summarise(dplyr::across(.cols = c(orig, nova, dif),
                                       .fns = sum, na.rm = TRUE),
                         .groups = "drop")

      return(mod_df)
    })

    dre_mod <- reactive({
      # req(district_mod())

      mod_df <- district_mod() |>
        dplyr::group_by(nm_dre) |>
        dplyr::summarise(dplyr::across(.cols = c(orig, nova, dif),
                                       .fns = sum, na.rm = TRUE),
                         .groups = "drop")

      return(mod_df)
    })

    # Modifications by area --------------------------------------------------------

    footer_total <- function(values) format(sum(values, na.rm = TRUE),
                                            big.mark = ".", decimal.mark = ",")

    output$table_mod <- reactable::renderReactable({
      req(nrow(sector_mod()) > 0, state$window_height)

      reactable::reactable(
        sector_mod(),
        compact = TRUE,
        defaultColDef = reactable::colDef(minWidth = 40, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        defaultPageSize = round((state$window_height - 220) / 31),  # 345
        paginationType = "simple",
        searchable = FALSE,
        resizable = TRUE,
        wrap = FALSE,
        # onClick = onclick_js,
        defaultSorted = list(#nm_dre = "asc", nm_distrito = "asc",
                             cd_setor = "asc"),
        # groupBy = c("nm_dre", "nm_distrito", "cd_setor"),
        # theme = reactable::reactableTheme(
        #   rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        # ),

        columns = list(
          nm_dre = reactable::colDef(name = "DRE", filterable = TRUE, show = TRUE),
          nm_distrito = reactable::colDef(name = "Distrito", filterable = TRUE, show = TRUE),
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE, show = TRUE),
          etapa = reactable::colDef(name = "Etapa", filterable = TRUE, show = TRUE),

          orig = reactable::colDef(name = "Atuais", footer = footer_total),
          nova = reactable::colDef(name = "Simulação", footer = footer_total),
          dif = reactable::colDef(name = "Diferença", footer = footer_total)

        ),
        columnGroups = list(
          reactable::colGroup(name = "Vagas", columns = c("orig", "nova", "dif"))
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
      req(hex_mod(), sector_mod(), input$filtro_etapa, input$var_mapa)

      if (input$unidade_espacial == "hexgrid") {
        return(map_data_hex())
      } else {
        return(map_data_setor())
      }

    })

    map_data_hex <- function() {
      # req(hex_mod(), input$filtro_etapa, input$var_mapa)

      # buscar dados por setor para plotar
      # etapas selecionadas pelo usuário
      etapas_selecionadas <- input$filtro_etapa
      etapas_selecionadas <- stringr::str_replace(etapas_selecionadas, "Fund. ", "Fundamental ")

      # variável selecionada para plotar
      var_selecionada <- switch (input$var_mapa,
                                 "Vagas Atuais" = "orig",
                                 "Vagas Simuladas" = "nova",
                                 "Diferença" = "dif"
      )

      # preparar o data.frame
      mod_df <- hex_mod() |>
        dplyr::filter(etapa %in% etapas_selecionadas) |>
        dplyr::mutate(highlight_var = get(var_selecionada)) |>
        dplyr::group_by(nm_dre, nm_distrito, cd_setor, id_hex) |>
        dplyr::summarise(orig = sum(orig), nova = sum(nova), dif = sum(dif),
                         highlight_var = sum(highlight_var),
                         .groups = "drop")

      return(mod_df)
      # map_data <- dplyr::left_join(hexgrid, mod_df, by = "id_hex")
      #
      # return(map_data)
    }

    map_data_setor <- function(){
      # req(sector_mod(), input$filtro_etapa, input$var_mapa)

      # buscar dados por setor para plotar
      # etapas selecionadas pelo usuário
      etapas_selecionadas <- input$filtro_etapa
      etapas_selecionadas <- stringr::str_replace(etapas_selecionadas, "Fundamental ", "Fund. ")

      # variável selecionada para plotar
      var_selecionada <- switch (input$var_mapa,
                                 "Vagas Atuais" = "orig",
                                 "Vagas Simuladas" = "nova",
                                 "Diferença" = "dif"
                                 )

      # preparar o data.frame
      mod_df <- sector_mod() |>
        dplyr::filter(etapa %in% etapas_selecionadas) |>
        dplyr::mutate(highlight_var = get(var_selecionada)) |>
        dplyr::group_by(nm_dre, nm_distrito, cd_setor) |>
        dplyr::summarise(orig = sum(orig), nova = sum(nova), dif = sum(dif),
                         highlight_var = sum(highlight_var),
                         .groups = "drop")

      return(mod_df)
#
#       map_data <- dplyr::left_join(setores, mod_df, by = c("cd_setor", "nm_distrito", "nm_dre"))
#
#       return(map_data)
    }

    # Modifications map --------------------------------------------------------
    output$map_mod <- mapdeck::renderMapdeck({
      sf_shape <- setores |>
        dplyr::mutate(popup = glue::glue("<div><b>DRE: </b>{nm_dre}</div> <div><b>Distrito: </b>{nm_distrito}</div> <div><b>Setor: </b>{cd_setor}</div>"))

      state$map_id <- ns("map_mod")

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

    legend_converter <- function (x) as.integer(x)

    observeEvent(
      eventExpr = {
        map_shape()
        map_data()
      },
      handlerExpr = {
        req(map_shape(), map_data())

        data_sf <- dplyr::left_join(map_shape(), map_data()) |>
          dplyr::mutate(valor = highlight_var)

        # create legend
        l_palette <- "viridis"
        l_values <- c(data_sf$valor)

        if (input$var_mapa == "Diferença") {
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


        if (input$var_mapa == "Diferença") {
          l$colours <- l$colours[2:(length(l$colours)-1)]
        }

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

        mapdeck::mapdeck_update(map_id = ns("map_mod")) |>
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
# mod_res_summary_area_ui("res_summary_area_1")

## To be copied in the server
# mod_res_summary_area_server("res_summary_area_1")
