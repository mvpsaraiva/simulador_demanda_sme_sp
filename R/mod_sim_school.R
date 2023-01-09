#' sim_school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_school_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(
    div(
      style = "margin: 10px; padding: 10px; height: 100%; width: 100%; overflow-y: auto",
      uiOutput(ns("school_details"))
    )
    # )
  )
}

#' sim_school Server Functions
#'
#' @noRd
mod_sim_school_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    escola <- reactive({
      prepare_school_data(escolas, state$school_selected)
    })

    # Headline ----------------------------------------------------------------
    footer_total <- reactable::JS(
      "function(column, state) {
          let total = 0
          state.sortedData.forEach(function(row) {
            total += row[column.id]
          })
          return total.toLocaleString(2)
        }"
    )

    output$school_details <- renderUI({
      req(state$school_selected)

      if (state$school_selected == -1) {
        # no school selected
        # div(
        #   style = "margin: 10px; padding: 10px; height: 100%; overflow-y: auto",
          tagList(
            h4("Nenhuma escola selecionada", class = "tile-headline")
          )
        # )

      } else {
        # build ui with school details

        # escola <- prepare_school_data(escolas, state$school_selected)
        deficit_setor <- prepare_sector_deficit_data(escolas, state$school_selected)
        deficit_distrito <- prepare_district_deficit_data(escolas, state$school_selected)

        tagList(
          div(
            # style = "margin: 10px; padding: 10px; overflow-y: auto",
            tagList(
              h4(paste0("Déficit / Superávit por Distrito - ", escola()$nm_distrito[1]), class = "tile-headline"),
              reactable::reactable({deficit_distrito},
                                   compact = TRUE,
                                   highlight = TRUE,
                                   pagination = FALSE,
                                   searchable = FALSE,
                                   resizable = TRUE,
                                   wrap = FALSE,
                                   fullWidth = TRUE,
                                   defaultColDef = reactable::colDef(minWidth = 50),
                                   columns = list(
                                     serie = reactable::colDef(name = "Série", sticky = "left", minWidth = 85,
                                                               style = list(borderRight = "1px solid #eee"),
                                                               headerStyle = list(borderRight = "1px solid #eee")),

                                     deficit_2020 = reactable::colDef(name = "2020", footer = footer_total),
                                     deficit_2035 = reactable::colDef(name = "2035", footer = footer_total),
                                     deficit_2045 = reactable::colDef(name = "2045", footer = footer_total),

                                     superavit_2020 = reactable::colDef(name = "2020",
                                                                        footer = footer_total,
                                                                        style = list(borderLeft = "1px solid #eee"),
                                                                        headerStyle = list(borderLeft = "1px solid #eee")),
                                     superavit_2035 = reactable::colDef(name = "2035", footer = footer_total),
                                     superavit_2045 = reactable::colDef(name = "2045", footer = footer_total)
                                   ),
                                   columnGroups = list(
                                     reactable::colGroup(name = "Déficit",
                                                         columns = c("deficit_2020", "deficit_2035", "deficit_2045")
                                     ),
                                     reactable::colGroup(name = "Superávit",
                                                         columns = c("superavit_2020", "superavit_2035", "superavit_2045")
                                     )
                                   )

              ),

              hr(),

              h4(paste0("Déficit / Superávit por Setor - ", escola()$cd_setor[1]), class = "tile-headline"),
              reactable::reactable({deficit_setor},
                                   compact = TRUE,
                                   highlight = TRUE,
                                   pagination = FALSE,
                                   searchable = FALSE,
                                   resizable = TRUE,
                                   wrap = FALSE,
                                   fullWidth = TRUE,
                                   defaultColDef = reactable::colDef(minWidth = 50),
                                   columns = list(
                                     serie = reactable::colDef(name = "Série", sticky = "left", minWidth = 85,
                                                               style = list(borderRight = "1px solid #eee"),
                                                               headerStyle = list(borderRight = "1px solid #eee")),

                                     deficit_2020 = reactable::colDef(name = "2020", footer = footer_total),
                                     deficit_2035 = reactable::colDef(name = "2035", footer = footer_total),
                                     deficit_2045 = reactable::colDef(name = "2045", footer = footer_total),

                                     superavit_2020 = reactable::colDef(name = "2020",
                                                                        footer = footer_total,
                                                                        style = list(borderLeft = "1px solid #eee"),
                                                                        headerStyle = list(borderLeft = "1px solid #eee")),
                                     superavit_2035 = reactable::colDef(name = "2035", footer = footer_total),
                                     superavit_2045 = reactable::colDef(name = "2045", footer = footer_total)
                                   ),
                                   columnGroups = list(
                                     reactable::colGroup(name = "Déficit",
                                                         columns = c("deficit_2020", "deficit_2035", "deficit_2045")
                                     ),
                                     reactable::colGroup(name = "Superávit",
                                                         columns = c("superavit_2020", "superavit_2035", "superavit_2045")
                                     )
                                   )
              ),

              hr(),

              # dados da escola
              h4(escola()$no_entidade[1], class = "tile-headline"),
              h5(paste0(escola()$co_entidade[1], " | ", escola()$ds_endereco[1]), class = "tile-subheadline"),
              renderTable({ escola() |> dplyr::select(info, valor)}, colnames = FALSE, width = "100%"),
            ),
            div(
              style = "margin: auto;height: 50px; display: flex; justify-content: center; align-items: center;",
              actionButton(ns("btn_edit_school"), label = "Editar Capacidade da Escola")
            )
          )
        )
      }
    })

# Edição da escola --------------------------------------------------------

    observeEvent(input$btn_edit_school, {
      req(escola())

      # carregar informações da escola a ser editata
      state$edit_school <- list(
        co_entidade = escola()$co_entidade[1],
        no_entidade = escola()$no_entidade[1],

        orig_mat_creche = escola()$qt_mat_inf_cre[1],
        nova_mat_creche = escola()$qt_mat_inf_cre[1],

        orig_mat_pre = escola()$qt_mat_inf_pre[1],
        nova_mat_pre = escola()$qt_mat_inf_pre[1],

        orig_mat_fund_ai = escola()$qt_mat_fund_ai[1],
        nova_mat_fund_ai = escola()$qt_mat_fund_ai[1],

        orig_mat_fund_af = escola()$qt_mat_fund_af[1],
        nova_mat_fund_af = escola()$qt_mat_fund_af[1]
      )

      # se a escola já foi modificada nesta seção, carregar esses dados
      school_mod <- state$school_mod |>
        dplyr::filter(co_entidade == escola()$co_entidade[1])

      if (nrow(school_mod) > 0) {
        state$edit_school$nova_mat_creche <- school_mod$nova_mat_creche
        state$edit_school$nova_mat_pre <- school_mod$nova_mat_pre
        state$edit_school$nova_mat_fund_ai <- school_mod$nova_mat_fund_ai
        state$edit_school$nova_mat_fund_af <- school_mod$nova_mat_fund_af
      }

      showModal(modalDialog(
        size = "s",
        easyClose = TRUE,
        mod_sim_edit_school_ui("sim_edit_school"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_save_changes"), "Salvar")
        )
      ))
    })

    observeEvent(input$btn_save_changes, {
      # dataframe no formato da tabela de modificações
      escolas_mod = data.frame(co_entidade = state$edit_school$co_entidade,
                               no_entidade = state$edit_school$no_entidade,
                               orig_mat_creche = state$edit_school$orig_mat_creche,
                               nova_mat_creche = state$edit_school$nova_mat_creche,

                               orig_mat_pre = state$edit_school$orig_mat_pre,
                               nova_mat_pre = state$edit_school$nova_mat_pre,

                               orig_mat_fund_ai = state$edit_school$orig_mat_fund_ai,
                               nova_mat_fund_ai = state$edit_school$nova_mat_fund_ai,

                               orig_mat_fund_af = state$edit_school$orig_mat_fund_af,
                               nova_mat_fund_af = state$edit_school$nova_mat_fund_af
      )

      state$school_mod <- rbind(state$school_mod, escolas_mod)

      removeModal()
    })

  })
}
