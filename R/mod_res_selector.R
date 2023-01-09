#' res_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_selector_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("table_scenarios"), width = "100%", height = "100%")
  )
}

#' res_selector Server Functions
#'
#' @noRd
mod_res_selector_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df_cenarios <- reactive({
      req(state$db_con, state$new_scenario)

      if (DBI::dbExistsTable(state$db_con, "cenarios")) {
        DBI::dbReadTable(state$db_con, "cenarios")
      } else {
        data.frame(id = numeric(),
                   data = numeric(),
                   nome = character(),
                   autor = character(),
                   descricao = character()
                   )
      }
    })

    selected_row <- reactive({
      req(df_cenarios())

      reactable::getReactableState("table_scenarios", "selected")
    })

    observeEvent(selected_row(), {
      req(selected_row())
      row_index <- selected_row()

      if (is.null(row_index)) {
        state$scenario_selected <- -1
      } else {
        selected_scenario <- df_cenarios()[row_index,]
        state$scenario_selected <- selected_scenario$id[1]
      }
    })

    output$table_scenarios <- reactable::renderReactable({
      req(df_cenarios(), state$window_height)

      reactable::reactable(
        df_cenarios(),
        compact = TRUE,
        defaultColDef = reactable::colDef(minWidth = 30, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        selection = "single",
        pagination = FALSE,
        # defaultPageSize = 10, # round((state$window_height - 220) / 31),  # 345
        # paginationType = "simple",
        # searchable = TRUE,
        wrap = FALSE,
        onClick = "select", # onclick_js,
        defaultSorted = list(data = "asc"),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        ),

        columns = list(
          .selection = reactable::colDef(show = FALSE),
          id = reactable::colDef(name = "Id", show = TRUE, filterable = FALSE, minWidth = 10),
          data = reactable::colDef(name = "Data", show = FALSE),
          nome = reactable::colDef(name = "Cenário", filterable = TRUE, minWidth = 50),
          autor = reactable::colDef(name = "Autor", filterable = TRUE, minWidth = 30),
          descricao = reactable::colDef(name = "Descricao", show = FALSE)
        ),
        language = reactable::reactableLang(
          searchPlaceholder = "Pesquisar cenário",
          noData = "Nenhum cenário",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} cenários",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}"
        )
      )
    })


  })
}

## To be copied in the UI
# mod_res_selector_ui("res_selector_1")

## To be copied in the server
# mod_res_selector_server("res_selector_1")
