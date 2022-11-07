#' resultados UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_resultados_ui <- function(id){
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c("    1fr   ",
                 "1fr data  "),
      gridlayout::grid_card
      ("data",
        tabsetPanel(
          tabPanel(title = "Cenários",
                   gridlayout::grid_container(
                     layout = c("    300px  1fr ",
                                "1fr text   data"),
                     gridlayout::grid_card_text("text",
                                                "Selecione um dos cenários de simulação na tabela ao lado, e explore os resultados nas abas acima: Por Distrito, Por Setor, e o mapeamento por Hexágonos.",
                                                wrapping_tag = "p"
                                                ),
                     gridlayout::grid_card("data",
                                           DT::dataTableOutput(ns("tabela_cenarios")))
                   )),
          tabPanel(title = "Por Distrito", DT::dataTableOutput(ns("tabela_deficit_distrito"))),
          tabPanel(title = "Por Setor", DT::dataTableOutput(ns("tabela_deficit_setor")))
          # tabPanel(title = "Por Hexágono")
        )
      )
    )
  )
}

#' resultados Server Functions
#'
#' @noRd
mod_resultados_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Dados pré-carregados - cenários já calculados ------------------------------------

    cenarios <- reactiveValues(
      data = DBI::dbReadTable(db_con, "cenarios") |>
        dplyr::select(-data)
    )

    output$tabela_cenarios <-  DT::renderDataTable({

      # container com formatação extra para a tabela
      sketch = htmltools::withTags(table(class = 'display',
                                         thead(
                                           tr(
                                             th("Cenário"),
                                             th("Nome"),
                                             th("Autor"),
                                             th('Descrição')
                                           )
                                         )))

      DT::datatable(cenarios$data,
                    rownames = FALSE,
                    container = sketch,
                    selection = "single",
                    extensions = c("Responsive", "Scroller"),
                    list(
                      lengthMenu = c(5, 10, 15, 30, 50),
                      pageLength = 15
                    )
      )
    })

    cenario_atual <- reactive({
      if (length(input$tabela_cenarios_rows_selected > 0)) {
        cenario_selecionado <- cenarios$data[input$tabela_cenarios_rows_selected[1],]


        return(cenario_selecionado)
      } else {
        return(dplyr::filter(cenarios$data, id == -1))
      }
    })

    deficit_por_distritos_df <- reactive({
      id_cenario <- cenario_atual()$id[1]

      def_distrito <-
        DBI::dbGetQuery(db_con,
                        sprintf("SELECT * FROM deficit_por_distrito WHERE id_cenario = %d AND cutoff = 15", id_cenario))

      def_distrito <- def_distrito |>
        dplyr::select(-cd_distrito, -faixa_idade, -etapa, -cutoff, -populacao, -matriculas, -vagas_acessiveis) |>
        tidyr::pivot_wider(names_from = serie, values_from = deficit)

      return(def_distrito)
    })

    deficit_por_setores_df <- reactive({
      id_cenario <- cenario_atual()$id[1]

      def_setores <-
        DBI::dbGetQuery(db_con,
                        sprintf("SELECT * FROM deficit_por_setor WHERE id_cenario = %d AND cutoff = 15", id_cenario))

      def_setores <- def_setores |>
        dplyr::select(-cd_distrito, -faixa_idade, -etapa, -cutoff, -populacao, -matriculas, -vagas_acessiveis) |>
        tidyr::pivot_wider(names_from = serie, values_from = deficit)


      return(def_setores)
    })

    output$tabela_deficit_distrito <-  DT::renderDataTable({

      # container com formatação extra para a tabela
      sketch = htmltools::withTags(table(class = 'display',
                                         thead(
                                           tr(
                                             th(rowspan = 2, "Cenário"),
                                             th(rowspan = 2, "DRE"),
                                             th(rowspan = 2, colspan = 2, "Distrito"),
                                             th(rowspan = 2, 'Ano da Previsão'),
                                             th(colspan = 4, 'Déficit ou Superávit de Vagas')
                                           ),
                                           tr(
                                             th("Creche"),
                                             th("Pré-Escola"),
                                             th("Fundamental I"),
                                             th("Fundamental II")
                                           )
                                         )))

      DT::datatable(deficit_por_distritos_df(),
                    container = sketch,
                    rownames = FALSE,
                    selection = "single",
                    extensions = c("Responsive", "Scroller"),
                    list(
                      lengthMenu = c(5, 10, 15, 30, 50),
                      pageLength = 15
                    )
      )
    })

    output$tabela_deficit_setor <-  DT::renderDataTable({
      sketch = htmltools::withTags(table(class = 'display',
                                         thead(
                                           tr(
                                             th(rowspan = 2, "Cenário"),
                                             th(rowspan = 2, "DRE"),
                                             th(rowspan = 2, colspan = 2, "Distrito"),
                                             th(rowspan = 2, 'Setor'),
                                             th(rowspan = 2, 'Ano da Previsão'),
                                             th(colspan = 4, 'Déficit ou Superávit de Vagas')
                                           ),
                                           tr(
                                             th("Creche"),
                                             th("Pré-Escola"),
                                             th("Fundamental I"),
                                             th("Fundamental II")
                                           )
                                         )))

            DT::datatable(deficit_por_setores_df(),
                    rownames = FALSE,
                    container = sketch,
                    selection = "single",
                    extensions = c("Responsive", "Scroller"),
                    list(
                      lengthMenu = c(5, 10, 15, 30, 50),
                      pageLength = 15
                    )
      )
    })


  })
}

## To be copied in the UI
# mod_resultados_ui("resultados_1")

## To be copied in the server
# mod_resultados_server("resultados_1")
