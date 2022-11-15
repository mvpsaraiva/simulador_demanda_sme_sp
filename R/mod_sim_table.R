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
    div(
      style = "margin: 5px -5px 0 -5px; height: calc(100% - 1px)",
      reactable::reactableOutput(ns("table_schools"), height = "100%")
    )
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

      if (state$state$id == STATE_MB_SELECTED) {
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
      # req(d_table(), state$window_height)
      req(state$window_height)

      reactable::reactable(
        df_escolas(),
        compact = TRUE,
        defaultColDef = reactable::colDef(minWidth = 30, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        defaultPageSize = round((state$window_height - 345) / 31),
        paginationType = "simple",
        searchable = TRUE,
        wrap = FALSE,
        onClick = onclick_js,
        defaultSorted = list(cd_setor = "asc", no_entidade = "asc"),

        columns = list(
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE),
          nm_distrito = reactable::colDef(name = "Distrito", show = FALSE),
          co_entidade = reactable::colDef(name = "Código (MEC)", filterable = TRUE),
          no_entidade = reactable::colDef(name = "Nome", filterable = TRUE, class = "area-link"),
          tp_categoria = reactable::colDef(name = "Dependência", filterable = TRUE),
          qt_mat_inf_cre = reactable::colDef(name = "Mat. Creche", header = icon("car"), footer = footer_total),
          qt_mat_inf_pre = reactable::colDef(name = "Mat. Pré-escola", header = icon("train"), footer = footer_total),
          qt_mat_fund_ai = reactable::colDef(name = "Mat. Fundamental I", header = icon("walking"), footer = footer_total),
          qt_mat_fund_af = reactable::colDef(name = "Mat. Fundamental II", header = icon("bicycle"), footer = footer_total)
        ),
        language = reactable::reactableLang(
          searchPlaceholder = "Filtrar escolas",
          noData = "Nenhuma escola encontrada",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} escolas",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}"
        )
      )
    })

    observeEvent(input$show_details, {
      req(input$show_details)

      selected_row <- df_escolas()[input$show_details$index,]

      # change the app state
      state$school_selected <- selected_row$co_entidade
    })


  })
}

## To be copied in the UI
# mod_sim_table_ui("sim_table_1")

## To be copied in the server
# mod_sim_table_server("sim_table_1")
