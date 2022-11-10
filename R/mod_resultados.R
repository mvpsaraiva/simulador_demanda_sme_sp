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
        tabsetPanel(id = ns("tab_resultados"),
          tabPanel(title = "Cenários",
                   gridlayout::grid_container(
                     layout = c("      300px   1fr ",
                                "400px text    data",
                                "100px btn_xl  data"
                     ),
                     gridlayout::grid_card_text("text",
                                                "Selecione um dos cenários de simulação na tabela ao lado, e explore os resultados nas abas acima: Por Distrito, Por Setor, e o mapeamento por Hexágonos.",
                                                wrapping_tag = "p"),
                     gridlayout::grid_card("btn_xl",
                                           downloadButton(ns("download_xlsx"), label = "Exportar resultados")),
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
mod_resultados_server <- function(id, db_con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Dados pré-carregados - cenários já calculados ------------------------------------

    cenarios <- reactive({
      input$tab_resultados

      if (DBI::dbExistsTable(db_con, "cenarios")) {
        DBI::dbReadTable(db_con, "cenarios") |>
          dplyr::select(-data)
      } else {
        data.frame()
      }
    })

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

      DT::datatable(cenarios(),
                    rownames = FALSE,
                    container = sketch,
                    selection = "single",
                    extensions = c("Scroller"),
                    list(
                      # lengthMenu = c(5, 10, 15, 30, 50),
                      # pageLength = 15
                      deferRender = TRUE,
                      scrollX = TRUE,
                      scrollY = "50vh",
                      scroller = TRUE
                    )
      )
    })

    cenario_atual <- reactive({
      if (length(input$tabela_cenarios_rows_selected > 0)) {
        cenario_selecionado <- cenarios()[input$tabela_cenarios_rows_selected[1],]

        return(cenario_selecionado)
      } else {
        return(dplyr::filter(cenarios(), id == -1))
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


    output$download_xlsx <- downloadHandler(
      filename = function() {
        id_cenario <- cenario_atual()$id[1]
        xls_file <- paste0("resultados_", id_cenario, ".xlsx")
      },

      content = function(file) {
        id_cenario <- cenario_atual()$id[1]

        # Carrega dados do cenário simulado
        cenario <- DBI::dbGetQuery(db_con,
                                   sprintf("SELECT * FROM cenarios WHERE id = %d", id_cenario)) |>
          dplyr::mutate(data = as.Date(as.POSIXct(data, origin="1970-01-01")))

        modificacoes <- DBI::dbGetQuery(db_con,
                                        sprintf("SELECT * FROM modificacoes WHERE id_cenario = %d", id_cenario)) |>
          dplyr::mutate(add_mat_creche = nova_mat_creche - orig_mat_creche,
                        add_mat_pre = nova_mat_pre - orig_mat_pre,
                        add_mat_fund_ai = nova_mat_fund_ai - orig_mat_fund_ai,
                        add_mat_fund_af = nova_mat_fund_af - orig_mat_fund_af) |>
          dplyr::select(id_cenario, co_entidade, no_entidade,
                        orig_mat_creche, nova_mat_creche, add_mat_creche,
                        orig_mat_pre, nova_mat_pre, add_mat_pre,
                        orig_mat_fund_ai, nova_mat_fund_ai, add_mat_fund_ai,
                        orig_mat_fund_af, nova_mat_fund_af, add_mat_fund_af)

        deficit_por_hex <- DBI::dbGetQuery(db_con,
                                           sprintf("SELECT * FROM deficit_por_hex WHERE id_cenario = %d AND cutoff = 15", id_cenario))

        deficit_por_setor <- DBI::dbGetQuery(db_con,
                                             sprintf("SELECT * FROM deficit_por_setor WHERE id_cenario = %d AND cutoff = 15", id_cenario))

        deficit_por_distrito <- DBI::dbGetQuery(db_con,
                                                sprintf("SELECT * FROM deficit_por_distrito WHERE id_cenario = %d AND cutoff = 15", id_cenario))

        # Carrega dados de déficit atual
        deficit_por_hex_orig <- DBI::dbGetQuery(db_con, "SELECT * FROM deficit_bfca_hex WHERE cutoff = 15")
        deficit_por_setor_orig <- DBI::dbGetQuery(db_con, "SELECT * FROM deficit_bfca_setor WHERE cutoff = 15")
        deficit_por_distrito_orig <- DBI::dbGetQuery(db_con, "SELECT * FROM deficit_bfca_distrito WHERE cutoff = 15")

        # Preparar dados para Excel
        deficit_hex_joined <- dplyr::inner_join(deficit_por_hex_orig, deficit_por_hex,
                                                suffix = c("_orig", "_sim"),
                                                by = c("id_hex", "ano", "faixa_idade", "etapa", "serie", "cutoff"))

        deficit_hex_joined <- deficit_hex_joined |>
          dplyr::select(id_hex, nr_distrito, nm_distrito, cd_setor,
                        ano, faixa_idade, etapa, serie,
                        populacao = populacao_orig, matriculas = matriculas_orig,
                        vagas_acessiveis_orig, deficit_orig,
                        vagas_acessiveis_sim, deficit_sim) |>
          dplyr::arrange(cd_setor, faixa_idade, ano, id_hex)

        deficit_setor_joined <- dplyr::inner_join(deficit_por_setor_orig, deficit_por_setor,
                                                  by = c("cd_setor", "ano", "faixa_idade", "etapa", "serie", "cutoff"),
                                                  suffix = c("_orig", "_sim"))


        deficit_setor_joined <- deficit_setor_joined |>
          dplyr::select(cd_dre = cd_dre_orig, nr_distrito = nr_distrito_orig, nm_distrito = nm_distrito_orig,
                        cd_setor, ano, faixa_idade, etapa, serie,
                        populacao = populacao_orig, matriculas = matriculas_orig,
                        vagas_acessiveis_orig, deficit_orig,
                        vagas_acessiveis_sim , deficit_sim)


        deficit_distrito_joined <- dplyr::inner_join(deficit_por_distrito_orig, deficit_por_distrito,
                                                     by = c("cd_dre", "nr_distrito", "nm_distrito",
                                                            "ano", "faixa_idade", "etapa", "serie", "cutoff"),
                                                     suffix = c("_orig", "_sim"))

        deficit_distrito_joined <- deficit_distrito_joined |>
          dplyr::select(cd_dre, nr_distrito, nm_distrito,
                        ano, faixa_idade, etapa, serie,
                        populacao = populacao_orig, matriculas = matriculas_orig,
                        vagas_acessiveis_orig, deficit_orig,
                        vagas_acessiveis_sim, deficit_sim)

        template_xlsx <- XLConnect::loadWorkbook("data/template_resultados.xlsx")

        XLConnect::readWorksheet(template_xlsx, "raw_cenario")

        XLConnect::writeWorksheet(template_xlsx, cenario, "raw_cenario")
        XLConnect::writeWorksheet(template_xlsx, modificacoes, "raw_modificacoes")
        XLConnect::writeWorksheet(template_xlsx, deficit_distrito_joined, "raw_deficit_distrito")
        XLConnect::writeWorksheet(template_xlsx, deficit_setor_joined, "raw_deficit_setor")
        XLConnect::writeWorksheet(template_xlsx, deficit_hex_joined, "raw_deficit_hex")

        xls_file <- paste0("data/resultados_", id_cenario, ".xlsx")
        XLConnect::saveWorkbook(template_xlsx, file)

      })



  })
}

## To be copied in the UI
# mod_resultados_ui("resultados_1")

## To be copied in the server
# mod_resultados_server("resultados_1")
