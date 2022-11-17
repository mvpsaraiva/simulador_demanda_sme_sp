#' sim_run UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_run_ui <- function(id){
  ns <- NS(id)
  tagList(

    gridlayout::grid_container(
      layout = c("       800px  400px      ",
                 "600px  table  scenario "
      ),
      gridlayout::grid_card(
        "table",
        h4("Tabela de modificações", class = "tile-headline"),
        reactable::reactableOutput(ns("table_modified_schools"), width = "100%", height = "100%")
      ),
      gridlayout::grid_card(
        "scenario",
        h4("Cenário", class = "tile-headline"),
        textInput(inputId = ns("cenario_nome"),
                  label = "Cenário",
                  value = "",
                  placeholder = "Cenário",
                  width = "100%"),
        textInput(inputId = ns("cenario_autor"),
                  label = "Autor",
                  placeholder = "Autor",
                  width = "100%"),
        textAreaInput(inputId = ns("cenario_descricao"),
                      label = "Descricao",
                      placeholder = "Descrição do cenário",
                      width="100%", height = "400px")
      )
    )
  )

}

#' sim_run Server Functions
#'
#' @noRd
mod_sim_run_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    school_mod <- reactive({
      state$school_mod
    })

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
        searchable = TRUE,
        wrap = FALSE,
        # onClick = onclick_js,
        defaultSorted = list(no_entidade = "asc"),
        # theme = reactable::reactableTheme(
        #   rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        # ),

        columns = list(
          co_entidade = reactable::colDef(name = "Código (MEC)", filterable = FALSE, show = FALSE),
          no_entidade = reactable::colDef(name = "Nome", filterable = FALSE, class = "area-link", minWidth = 150),
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

