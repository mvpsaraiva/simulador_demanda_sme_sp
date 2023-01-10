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

    make_xl_formula <- function(xl_function = "SUM", i_col, start_row, end_row) {
      formula = glue::glue("{xl_function}({i_col}{start_row}:{i_col}{end_row})")

    }

    output$download_xlsx <- downloadHandler(
      filename = function() {
        id_cenario <- cenario_atual()
        xls_file <- paste0("resultados_", id_cenario, ".xlsx")
      },

      content = function(file) {
        id_cenario <- cenario_atual()

        temp_xls <- file
        file.copy("data/template_resultados.xlsx", temp_xls)

        # carrega DRE para referencia
        dre_df <- sf::st_set_geometry(setores, NULL) |>
          dplyr::select(cd_dre, nm_dre) |>
          dplyr::distinct()


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

        adicoes <- DBI::dbGetQuery(state$db_con,
                                        sprintf("SELECT * FROM adicoes WHERE id_cenario = %d", id_cenario)) |>
          dplyr::mutate(tp_categoria = "Adicional",
                        orig_mat_creche = 0,
                        nova_mat_creche = qt_mat_inf_cre,
                        add_mat_creche = qt_mat_inf_cre,

                        orig_mat_pre = 0,
                        nova_mat_pre = qt_mat_inf_pre,
                        add_mat_pre = qt_mat_inf_pre,

                        orig_mat_fund_ai = 0,
                        nova_mat_fund_ai = qt_mat_fund_ai,
                        add_mat_fund_ai = qt_mat_fund_ai,

                        orig_mat_fund_af = 0,
                        nova_mat_fund_af = qt_mat_fund_af,
                        add_mat_fund_af = qt_mat_fund_af) |>
          dplyr::select(co_entidade, no_entidade, tp_categoria,
                        cd_dre, nm_dre, nr_distrito, nm_distrito, cd_setor,
                        orig_mat_creche, add_mat_creche, nova_mat_creche,
                        orig_mat_pre, add_mat_pre, nova_mat_pre,
                        orig_mat_fund_ai, add_mat_fund_ai, nova_mat_fund_ai,
                        orig_mat_fund_af, add_mat_fund_af, nova_mat_fund_af)

        modificacoes <- rbind(modificacoes, adicoes)

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
                                                    suffix = c("_sim", "_or")) |>
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
          dplyr::summarise(dplyr::across(.cols = c(populacao,
                                                   vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                                                   deficit_or, deficit_sim, deficit_rel,
                                                   superavit_or, superavit_sim, superavit_rel),
                                         .fns = sum, na.rm = TRUE), .groups = "drop")

        # prepara dataframe wide para Excel - Setor
        deficit_por_setor_wide <- deficit_por_setor |>
          dplyr::left_join(dre_df, by = "cd_dre") |>
          tidyr::pivot_wider(names_from = ano, values_from = c(populacao,
                                                               vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                                                               deficit_or, deficit_sim, deficit_rel,
                                                               superavit_or, superavit_sim, superavit_rel)) |>
          dplyr::select(cd_dre, nm_dre, nr_distrito, nm_distrito, cd_setor, serie, dplyr::ends_with("2020"), dplyr::ends_with("2035"), dplyr::ends_with("2045")) |>
          dplyr::mutate(serie = factor(serie,
                                       levels = c("creche", "pre", "anos_iniciais", "anos_finais"),
                                       labels = c("Creche", "Pré-escola", "Fundamental I", "Fundamental II"))) |>
          tidyr::drop_na()


        deficit_por_distrito <- deficit_por_setor |>
          dplyr::group_by(id_cenario, nr_distrito, nm_distrito, cd_dre,
                          ano, faixa_idade, etapa, serie) |>
          dplyr::summarise(dplyr::across(.cols = c(populacao,
                                                   vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                                                   deficit_or, deficit_sim, deficit_rel,
                                                   superavit_or, superavit_sim, superavit_rel),
                                         .fns = sum, na.rm = TRUE), .groups = "drop")

        # prepara dataframe wide para Excel - Distrito
        deficit_por_distrito_wide <- deficit_por_distrito |>
          dplyr::left_join(dre_df, by = "cd_dre") |>
          tidyr::pivot_wider(names_from = ano, values_from = c(populacao,
                                                               vagas_acessiveis_or, vagas_acessiveis_sim, vagas_acessiveis_rel,
                                                               deficit_or, deficit_sim, deficit_rel,
                                                               superavit_or, superavit_sim, superavit_rel)) |>
          dplyr::select(cd_dre, nm_dre, nr_distrito, nm_distrito, serie, dplyr::ends_with("2020"), dplyr::ends_with("2035"), dplyr::ends_with("2045")) |>
          dplyr::mutate(serie = factor(serie,
                                       levels = c("creche", "pre", "anos_iniciais", "anos_finais"),
                                       labels = c("Creche", "Pré-escola", "Fundamental I", "Fundamental II"))) |>
          tidyr::drop_na()


        template_xl <- openxlsx::loadWorkbook(temp_xls)

        openxlsx::writeData(template_xl, sheet = "r_cenario", x = cenario)

        # Modificações
        st_row = 8
        en_row = st_row + nrow(modificacoes) - 1
        fn_row = en_row + 1
        openxlsx::writeData(template_xl, sheet = "Modificações", x = modificacoes, colNames = FALSE, startRow = st_row)

        # totals for columns I (9) to T (20)
        openxlsx::writeData(template_xl, sheet = "Modificações", x = "Totais", startRow = fn_row, startCol = 8)
        formulas <- lapply(LETTERS[9:20], make_xl_formula, xl_function = "SUM", start_row = st_row, end_row = en_row) |> unlist()
        for (i in 9:20) {
          openxlsx::writeFormula(template_xl, sheet = "Modificações", x = formulas[i-8], startRow = fn_row, startCol = i)
        }

        # formatação das bordas
        st_thick_border <- openxlsx::createStyle(border = "Left", borderStyle = "medium")
        st_thick_border_bottom <- openxlsx::createStyle(border = "Bottom", borderStyle = "medium")
        st_thick_border_bold <- openxlsx::createStyle(border = "TopBottom", borderStyle = "medium", textDecoration = "bold")

        ## blocos de dados, borda grossa com fonte normal
        ### bloco Escola
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 1, gridExpand = TRUE)
        ### bloco DRE
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 4, gridExpand = TRUE)
        ### bloco Distrito
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 6, gridExpand = TRUE)
        ### bloco Setor
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:(en_row + 1), cols = 8, gridExpand = TRUE)
        ### bloco Creche
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 9, gridExpand = TRUE)
        ### bloco Pré-escola
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 12, gridExpand = TRUE)
        ### bloco Fundamental I
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 15, gridExpand = TRUE)
        ### bloco Fundamental II
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:en_row, cols = 18, gridExpand = TRUE)

        ## linha de totais, borda grossa com fonte em negrito
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border_bold, rows = fn_row, cols = 8:20, gridExpand = TRUE, stack = TRUE)

        ### fechamento
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border, rows = st_row:(en_row + 1), cols = 21, gridExpand = TRUE)
        openxlsx::addStyle(template_xl, sheet = "Modificações", style = st_thick_border_bottom, rows = en_row, cols = 1:7, gridExpand = TRUE, stack = TRUE)

        # Resultados por Setor
        openxlsx::writeData(template_xl, sheet = "Resultados por Setor", x = deficit_por_setor_wide, startRow = 8, colNames = FALSE)

        # Resultados por Distrito
        openxlsx::writeData(template_xl, sheet = "Resultados por Distrito", x = deficit_por_distrito_wide, startRow = 8, colNames = FALSE)


        # openxlsx::writeData(template_xl, sheet = "r_deficit_distrito", x = deficit_por_distrito)
        # openxlsx::writeData(template_xl, sheet = "r_deficit_setor", x = deficit_por_setor)

        openxlsx::saveWorkbook(template_xl, temp_xls, overwrite = TRUE)
      })

  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
