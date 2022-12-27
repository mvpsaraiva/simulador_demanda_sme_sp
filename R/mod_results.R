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
          tabPanel(title = "Déficit / Superávit)",
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
          dplyr::left_join(escolas) |>
          dplyr::mutate(add_mat_creche = nova_mat_creche - orig_mat_creche,
                        add_mat_pre = nova_mat_pre - orig_mat_pre,
                        add_mat_fund_ai = nova_mat_fund_ai - orig_mat_fund_ai,
                        add_mat_fund_af = nova_mat_fund_af - orig_mat_fund_af) |>
          dplyr::select(co_entidade, no_entidade, tp_categoria,
                        cd_dre, nm_dre, nr_distrito, nm_distrito, cd_setor,
                        orig_mat_creche, add_mat_creche, nova_mat_creche,
                        orig_mat_pre, add_mat_pre, nova_mat_pre,
                        orig_mat_fund_ai, add_mat_fund_ai, nova_mat_fund_ai,
                        orig_mat_fund_af, add_mat_fund_af, nova_mat_fund_af)

        # Carrega déficit por hex simulado
        deficit_por_hex <- DBI::dbGetQuery(state$db_con,
                                           sprintf("SELECT * FROM deficit_por_hex WHERE id_cenario = %d AND cutoff = 15", id_cenario)) |>
          dplyr::mutate(superavit = purrr::map2_dbl(deficit, 0, max),
                        deficit = purrr::map2_dbl(deficit, 0, min)) |>
          dplyr::select(-populacao, -matriculas, -cutoff)


        # Carrega dados de déficit por hex atual
        deficit_por_hex_orig <- deficit_bfca_hex # DBI::dbGetQuery(state$db_con, "SELECT * FROM deficit_bfca_hex WHERE cutoff = 15")

        # Combina resultados simulados e atuais
        deficit_por_hex_joined <- dplyr::inner_join(deficit_por_hex, deficit_por_hex_orig,
                                                    by = c("id_hex", "ano", "faixa_idade", "etapa", "serie"),
                                                    suffix = c("_or", "_sim")) |>
          dplyr::mutate(vagas_acessiveis_rel = vagas_acessiveis_sim - vagas_acessiveis_or,
                        deficit_rel = deficit_sim - deficit_or,
                        superavit_rel = superavit_sim - superavit_or) |>
          dplyr::select(id_cenario, id_hex, nr_distrito, nm_distrito, cd_dre, cd_setor,
                        ano, faixa_idade, etapa, serie, populacao, matriculas,
                        vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                        deficit_or, deficit_sim, deficit_rel,
                        superavit_or, superavit_sim, superavit_rel)


        deficit_por_setor <- deficit_por_hex_joined |>
          dplyr::group_by(id_cenario, nr_distrito, nm_distrito, cd_dre, cd_setor,
                          ano, faixa_idade, etapa, serie) |>
          dplyr::summarise(dplyr::across(.cols = c(populacao, matriculas,
                                                   vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                                                   deficit_or, deficit_sim, deficit_rel,
                                                   superavit_or, superavit_sim, superavit_rel),
                                         .fns = sum, na.rm = TRUE), .groups = "drop")

        deficit_por_distrito <- deficit_por_setor |>
          dplyr::group_by(id_cenario, nr_distrito, nm_distrito, cd_dre,
                          ano, faixa_idade, etapa, serie) |>
          dplyr::summarise(dplyr::across(.cols = c(populacao, matriculas,
                                                   vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                                                   deficit_or, deficit_sim, deficit_rel,
                                                   superavit_or, superavit_sim, superavit_rel),
                                         .fns = sum, na.rm = TRUE), .groups = "drop")


        template_xl <- openxlsx::loadWorkbook(temp_xls)

        openxlsx::writeData(template_xl, sheet = "r_cenario", x = cenario)
        openxlsx::writeData(template_xl, sheet = "Modificações", x = modificacoes, colNames = FALSE, startRow = 8)
        openxlsx::writeData(template_xl, sheet = "r_deficit_distrito", x = deficit_por_distrito)
        openxlsx::writeData(template_xl, sheet = "r_deficit_setor", x = deficit_por_setor)

        openxlsx::saveWorkbook(template_xl, temp_xls, overwrite = TRUE)
      })

  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
