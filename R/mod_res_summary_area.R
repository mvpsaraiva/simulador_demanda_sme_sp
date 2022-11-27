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
                                    width = "200px"),
          shinyWidgets::pickerInput(ns("var_mapa"),
                                    label = "Mapear vagas",
                                    choices = c("Atuais", "Projetadas", "Diferença"),
                                    selected = "Diferença",
                                    inline = TRUE,
                                    width = "200px"),
          hr(),

          reactable::reactableOutput(ns("table_mod"), width = "100%", height = "calc(100vh - 100px)")
        )
      ),
      column(
        width = 6,
        div(
          style = "margins: 5px",
          mapboxer::mapboxerOutput(ns("map_mod"), height = "calc(100vh - 100px)")
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

      mod_df <- novo_escolas_mod_vazio()

      if (DBI::dbExistsTable(state$db_con, "modificacoes")) {
        mod_df <- DBI::dbGetQuery(state$db_con,
                                  sprintf("SELECT * FROM modificacoes WHERE id_cenario = %d",
                                          state$scenario_selected)
        )

        mod_df <- dplyr::left_join(mod_df, escolas, by = c("co_entidade", "no_entidade")) |>
          dplyr::select(nm_dre, nm_distrito, cd_setor, co_entidade, no_entidade, tp_categoria, tmi_metro,
                        orig_mat_creche, nova_mat_creche, orig_mat_pre,nova_mat_pre,
                        orig_mat_fund_ai, nova_mat_fund_ai, orig_mat_fund_af, nova_mat_fund_af) |>
          dplyr::arrange(nm_dre, nm_distrito, cd_setor, no_entidade)
      }

      return(mod_df)
    })

    sector_mod <- reactive({
      req(school_mod())

      mod_setor <- data.frame(
        nm_dre = character(),
        nm_distrito = character(),
        cd_setor = character(),
        etapa = character(),
        orig = numeric(),
        nova = numeric(),
        dif = numeric()
      )

      if (nrow(school_mod()) > 0 ) {
        mod_df <- school_mod() |>
          dplyr::group_by(nm_dre, nm_distrito, cd_setor) |>
          dplyr::summarise(dplyr::across(.cols = c(orig_mat_creche, nova_mat_creche,
                                                   orig_mat_pre,nova_mat_pre,
                                                   orig_mat_fund_ai, nova_mat_fund_ai,
                                                   orig_mat_fund_af, nova_mat_fund_af),
                                         .fns = sum, na.rm = TRUE),
                           .groups = "drop")

        mod_setor <- mod_df |>
          tidyr::pivot_longer(cols = orig_mat_creche:nova_mat_fund_af) |>
          tidyr::separate(col = name, into = c("cenario", "mat", "etapa"), extra = "merge") |>
          tidyr::pivot_wider(names_from = cenario, values_from = value) |>
          dplyr::select(-mat) |>
          dplyr::mutate(dif = nova - orig)

        mod_setor$etapa <- factor(mod_setor$etapa,
                                  levels = c("creche", "pre", "fund_ai", "fund_af"),
                                  labels = c("Creche", "Pré-Escola", "Fund. I", "Fund. II"))
      }

      return(mod_setor)
    })

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

          orig = reactable::colDef(name = "Vagas\nAtuais", footer = footer_total),
          nova = reactable::colDef(name = "Vagas\nProjetadas", footer = footer_total),
          dif = reactable::colDef(name = "Diferença", footer = footer_total)

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


    # Modifications map --------------------------------------------------------
    tooltip_mb <- paste("<div><b>Distrito: </b>{{nm_distrito}}</div>",
                        "<div><b>Setor: </b>{{cd_setor}}</div>",
                        "<div><b>Vagas atuais: </b>{{orig}}</div>",
                        "<div><b>Vagas projetadas: </b>{{nova}}</div>",
                        "<div><b>Diferença: </b>{{dif}}</div>"
    )
    tooltip_selected_mb <- "<div><b>Distrito: </b>{{nm_distrito}}</div> <div><b>Setor: </b>{{cd_setor}}</div>"
    tooltip_highlighted_mb <- "<div><b>Distrito: </b>{{nm_distrito}}</div> <div><b>Setor: </b>{{cd_setor}}</div>"

    map_data <- reactive({
      req(sector_mod(), input$filtro_etapa, input$var_mapa)

      # buscar dados por setor para plotar
      # etapas selecionadas pelo usuário
      etapas_selecionadas <- input$filtro_etapa
      etapas_selecionadas <- stringr::str_replace(etapas_selecionadas, "Fund. ", "Fundamental ")

      # variável selecionada para plotar
      var_selecionada <- switch (input$var_mapa,
                                 "Atuais" = "orig",
                                 "Projetadas" = "nova",
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

      map_data <- dplyr::left_join(setores, mod_df, by = c("cd_setor", "nm_distrito", "nm_dre"))

      return(map_data)
    })


    map_rendered <- reactiveVal(FALSE)
    output$map_mod <- mapboxer::renderMapboxer({
      # req(state$scenario_selected != -1)

      state$map_id <- ns("map_mod")
      sf_shape <- setores
      map_rendered(TRUE)

      mapboxer::mapboxer(style = "mapbox://styles/mapbox/light-v9", token = golem::get_golem_options("mapbox_token"))  |>

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

        mapboxer::fit_bounds(sf::st_bbox(sf_shape))
    })

    # redesenhar mapa caso a variável seja atualizada
    observeEvent(
      {
        map_data()
        state$scenario_selected
        input$filtro_etapa
        input$var_mapa
      },
      {
        req(map_rendered() == TRUE)

        if (nrow(map_data() |> tidyr::drop_na()) == 0) {
          # sem dados para exibir
          mapboxer::mapboxer_proxy(ns("map_mod")) |>
            mapboxer::set_data(data = setores[0, ], source_id = "mb") |>

            mapboxer::update_mapboxer()

          return()
        }

        # selecionar a paleta de cores
        pal_type <- NULL
        pal <- NULL
        legend.cols = NULL
        legend_breaks = NULL

        if (input$var_mapa %in% c("Atuais", "Projetadas")) {
          pal_type <- "Reds"

          max_val <- max(map_data()$highlight_var, na.rm = TRUE)
          pal <- scales::col_bin(pal_type, c(0, max_val), bins = 7, na.color = "#04040408" )

          legend.breaks = seq(0, max_val, length.out = 7) |> round()
          legend.cols = pal(legend.breaks)

        } else {
          pal_type <- "RdBu"

          max_val <- max(abs(map_data()$highlight_var), na.rm = TRUE)
          pal <- scales::col_bin(pal_type, c(-max_val, max_val), bins = 7, na.color = "#04040408" )

          legend.breaks = seq(-max_val, max_val, length.out = 7) |> round()
          legend.cols = pal(legend.breaks)
        }

        # aplicar paleta de cores aos dados
        map_data_sf <- map_data() |>
          dplyr::mutate(color = pal(highlight_var)) |>
          tidyr::drop_na()

        # push new data into the `highlight-mb` map layer
        # NOTE: we need to use `ns()` to retrieve the correct map object

        if (nrow(map_data_sf) > 0) {

          mapboxer::mapboxer_proxy(ns("map_mod")) |>
            mapboxer::set_data(data = map_data_sf, source_id = "mb") |>

            mapboxer::update_mapboxer()
        } else {
          # sem dados para exibir
          mapboxer::mapboxer_proxy(ns("map_mod")) |>
            mapboxer::set_data(data = setores[0, ], source_id = "mb") |>

            mapboxer::update_mapboxer()

        }

      })

  })
}

## To be copied in the UI
# mod_res_summary_area_ui("res_summary_area_1")

## To be copied in the server
# mod_res_summary_area_server("res_summary_area_1")
