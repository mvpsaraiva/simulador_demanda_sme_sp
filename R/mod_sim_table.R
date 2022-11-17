#' sim_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("table_schools"), width = "100%", height = "100%")
  )
}

#' sim_table Server Functions
#'
#' @noRd
mod_sim_table_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Area selection ----------------------------------------------------------

    filter_choices <- reactive({
      req(state$dre)

      d <- setores |>
        sf::st_set_geometry(NULL) |>
        dplyr::select()

      if (state$dre != app_states$INITIAL_DRE) {
        d <- d |> filter(nm_dre == state$dre)
      }

      setNames(d$code, d$name)
    })


    # Tabela de escolas -------------------------------------------------------

    ## Filter data -------------------------------
    df_escolas <- reactive({
      req(state$dre, state$state$id)

      # Todas as escolas
      d <- escolas

      # Filtrar DRE
      if (state$dre != app_states$INITIAL_DRE) {
        d <- d |>
          dplyr::filter(nm_dre == state$dre)
      }

      if (state$state$id == app_states$STATE_MB_SELECTED) {
        d <- d |>
          dplyr::filter(cd_setor == state$state$store$selected_mb)
      }

      # Limpar as colunas
      d <- d |>
        dplyr::select(cd_setor, nm_distrito, co_entidade, no_entidade, tp_categoria,
                      qt_mat_inf_cre, qt_mat_inf_pre, qt_mat_fund_ai, qt_mat_fund_af)


      return(d)

    })

    ## Render table -------------------------------

    onclick_js <- reactable::JS(
      glue::glue(
        "function(rowInfo, colInfo) {
          // Send the click event to Shiny, which will be available in input$show_details
          // Note that the row index starts at 0 in JavaScript, so we add 1
          if (window.Shiny) {
            Shiny.setInputValue('<<< ns('show_details') >>>', { index: rowInfo.index + 1, rnd: Math.random() })
          }

        }", .open = "<<<", .close = ">>>")
    )

    footer_total <- function(values) format(sum(values, na.rm = TRUE),
                                            big.mark = ".", decimal.mark = ",")

    output$table_schools <- reactable::renderReactable({
      req(df_escolas(), state$window_height)

      reactable::reactable(
        df_escolas(),
        compact = TRUE,
        defaultColDef = reactable::colDef(minWidth = 30, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        # selection = "single",
        defaultPageSize = round((state$window_height - 220) / 31),  # 345
        paginationType = "simple",
        searchable = TRUE,
        wrap = FALSE,
        onClick = onclick_js,
        defaultSorted = list(cd_setor = "asc", no_entidade = "asc"),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        ),

        columns = list(
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE),
          nm_distrito = reactable::colDef(name = "Distrito", show = FALSE),
          co_entidade = reactable::colDef(name = "Código (MEC)", filterable = TRUE, show = FALSE),
          no_entidade = reactable::colDef(name = "Nome", filterable = TRUE, class = "area-link", minWidth = 150),
          tp_categoria = reactable::colDef(name = "Rede", filterable = TRUE, minWidth = 35),
          qt_mat_inf_cre = reactable::colDef(name = "CRE", footer = footer_total),# header = icon("car")),
          qt_mat_inf_pre = reactable::colDef(name = "PRE", footer = footer_total),# header = icon("train")),
          qt_mat_fund_ai = reactable::colDef(name = "F I", footer = footer_total),# header = icon("walking")),
          qt_mat_fund_af = reactable::colDef(name = "F II", footer = footer_total)#, header = icon("bicycle"))
        ),
        language = reactable::reactableLang(
          searchPlaceholder = "Pesquisar escolas",
          noData = "Nenhuma escola encontrada",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} escolas",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}"
        )
      )
    })

    observeEvent(input$show_details, {
      req(df_escolas(), input$show_details)

      selected_row <- df_escolas()[input$show_details$index,]

      # change the app state
      state$school_selected <- selected_row$co_entidade[1]
    })


  })
}

## To be copied in the UI
# mod_sim_table_ui("sim_table_1")

## To be copied in the server
# mod_sim_table_server("sim_table_1")
