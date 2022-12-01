#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c("      280px         1fr    ",
                 "1fr   table         content",
                 "75px  buttons       content"
      ),
      gridlayout::grid_card(
        "table",
        mod_res_selector_ui("res_selector")
      ),
      gridlayout::grid_card(
        "buttons",
        div(
          style = "margin: auto; padding: auto; height: 100%; display: flex; justify-content: center; align-items: center;",
          downloadButton(ns("download_xlsx"), label = "Exportar resultados")
        )
      ),
      gridlayout::grid_card(
        "content",
        tabsetPanel(
          id = ns("tab_scenario_details"),
          tabPanel(title = "Modificações",
                   mod_res_summary_ui("res_summary")
          ),
          tabPanel(title = "Modificações por Área",
                   style = "margin: 5px; padding: 5px;",
                   mod_res_summary_area_ui("res_summary_area")
          ),
          tabPanel(title = "Resultado (déficit ou superávit)",
                   style = "margin: 5px; padding: 5px;",
                   mod_res_deficit_setor_ui("res_deficit_setor")
          )
        )
      )
    )
  )
}

#' results Server Functions
#'
#' @noRd
mod_results_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    cenario_atual <- reactive({
      req(state$scenario_selected)

      return(state$scenario_selected)
    })

    output$download_xlsx <- downloadHandler(
      filename = function() {
        id_cenario <- cenario_atual()
        xls_file <- paste0("resultados_", id_cenario, ".xlsx")
      },

      content = function(file) {
        id_cenario <- cenario_atual()

        temp_xls <- file
        file.copy("data/template_resultados.xlsx", temp_xls)

        # Carrega dados do cenário simulado
        cenario <- DBI::dbGetQuery(state$db_con,
                                   sprintf("SELECT * FROM cenarios WHERE id = %d", id_cenario)) |>
          dplyr::mutate(data = as.Date(as.POSIXct(data, origin="1970-01-01")))

        modificacoes <- DBI::dbGetQuery(state$db_con,
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

        # deficit_por_hex <- DBI::dbGetQuery(state$db_con,
        #                                    sprintf("SELECT * FROM deficit_por_hex WHERE id_cenario = %d AND cutoff = 15", id_cenario))

        deficit_por_setor <- DBI::dbGetQuery(state$db_con,
                                             sprintf("SELECT * FROM deficit_por_setor WHERE id_cenario = %d AND cutoff = 15", id_cenario))

        deficit_por_distrito <- DBI::dbGetQuery(state$db_con,
                                                sprintf("SELECT * FROM deficit_por_distrito WHERE id_cenario = %d AND cutoff = 15", id_cenario))

        # Carrega dados de déficit atual
        # deficit_por_hex_orig <- DBI::dbGetQuery(state$db_con, "SELECT * FROM deficit_bfca_hex WHERE cutoff = 15")
        deficit_por_setor_orig <- deficit_bfca_setor |> dplyr::filter(cutoff == 15) # DBI::dbGetQuery(state$db_con, "SELECT * FROM deficit_bfca_setor WHERE cutoff = 15")
        deficit_por_distrito_orig <- deficit_bfca_distrito |> dplyr::filter(cutoff == 15) # DBI::dbGetQuery(state$db_con, "SELECT * FROM deficit_bfca_distrito WHERE cutoff = 15")


        # Preparar dados para Excel
        # deficit_hex_joined <- dplyr::inner_join(deficit_por_hex_orig, deficit_por_hex,
        #                                         suffix = c("_orig", "_sim"),
        #                                         by = c("id_hex", "ano", "faixa_idade", "etapa", "serie", "cutoff"))
        #
        # deficit_hex_joined <- deficit_hex_joined |>
        #   dplyr::select(id_hex, nr_distrito, nm_distrito, cd_setor,
        #                 ano, faixa_idade, etapa, serie,
        #                 populacao = populacao_orig, matriculas = matriculas_orig,
        #                 vagas_acessiveis_orig, deficit_orig,
        #                 vagas_acessiveis_sim, deficit_sim) |>
        #   dplyr::arrange(cd_setor, faixa_idade, ano, id_hex)

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

        template_xl <- openxlsx::loadWorkbook(temp_xls)

        openxlsx::writeData(template_xl, sheet = "r_cenario", x = cenario)
        openxlsx::writeData(template_xl, sheet = "r_modificacoes", x = modificacoes)
        openxlsx::writeData(template_xl, sheet = "r_deficit_distrito", x = deficit_distrito_joined)
        openxlsx::writeData(template_xl, sheet = "r_deficit_setor", x = deficit_setor_joined)

        openxlsx::saveWorkbook(template_xl, temp_xls, overwrite = TRUE)
      })

  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
