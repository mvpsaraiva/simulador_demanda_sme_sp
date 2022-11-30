#' res_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("table_modified_schools"), width = "100%", height = "100%")
  )
}

#' res_summary Server Functions
#'
#' @noRd
mod_res_summary_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    school_mod <- reactive({
      req(state$scenario_selected)

      mod_df <- data.frame(nm_dre = character(),
                           nm_distrito = character(),
                           cd_setor = character(),
                           no_entidade = character()
                           )

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


# Modified schools --------------------------------------------------------

    footer_total <- function(values) format(sum(values, na.rm = TRUE),
                                            big.mark = ".", decimal.mark = ",")

    output$table_modified_schools <- reactable::renderReactable({
      req(school_mod(), state$window_height)

      reactable::reactable(
        school_mod(),
        compact = TRUE,
        defaultColDef = reactable::colDef(minWidth = 40, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        defaultPageSize = round((state$window_height - 220) / 31),  # 345
        paginationType = "simple",
        searchable = FALSE,
        wrap = FALSE,
        resizable = TRUE,
        # onClick = onclick_js,
        defaultSorted = list(nm_dre = "asc", nm_distrito = "asc", cd_setor = "asc", no_entidade = "asc"),
        # groupBy = c("nm_dre", "nm_distrito", "cd_setor"),
        # theme = reactable::reactableTheme(
        #   rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        # ),

        columns = list(
          nm_dre = reactable::colDef(name = "DRE", filterable = TRUE, show = TRUE),
          nm_distrito = reactable::colDef(name = "Distrito", filterable = TRUE, show = TRUE),
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE, show = TRUE),
          co_entidade = reactable::colDef(name = "Código (MEC)", filterable = TRUE, show = FALSE),
          no_entidade = reactable::colDef(name = "Escola", filterable = TRUE, class = "area-link", minWidth = 150),
          tp_categoria = reactable::colDef(name = "Rede", filterable = TRUE, show = TRUE),
          tmi_metro = reactable::colDef(name = "Metrô (min)", filterable = FALSE, show = TRUE),

          orig_mat_creche = reactable::colDef(name = "CRE orig", footer = footer_total),
          nova_mat_creche = reactable::colDef(name = "CRE nova", footer = footer_total),

          orig_mat_pre = reactable::colDef(name = "PRE orig", footer = footer_total),
          nova_mat_pre = reactable::colDef(name = "PRE nova", footer = footer_total),

          orig_mat_fund_ai = reactable::colDef(name = "F I orig", footer = footer_total),
          nova_mat_fund_ai = reactable::colDef(name = "F I nova", footer = footer_total),

          orig_mat_fund_af = reactable::colDef(name = "F II orig", footer = footer_total),
          nova_mat_fund_af = reactable::colDef(name = "F II nova", footer = footer_total)
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

  })
}

## To be copied in the UI
# mod_res_summary_ui("res_summary_1")

## To be copied in the server
# mod_res_summary_server("res_summary_1")
