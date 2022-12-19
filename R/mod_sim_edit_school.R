#' sim_edit_school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_edit_school_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        uiOutput(ns("school_header"))
      ),
      fluidRow(
        uiOutput(ns("school_area"))
      ),
      fluidRow(
        uiOutput(ns("school_capacity"))
      )
    )
  )
}

#' sim_edit_school Server Functions
#'
#' @noRd
mod_sim_edit_school_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    escola <- reactive({
      prepare_school_data(escolas, state$school_selected)
    })


# Build UI ----------------------------------------------------------------

    output$school_header <- renderUI({
      req(escola())

      tagList(
        h4(escola()$no_entidade[1], class = "tile-headline"),
        h5(paste0(escola()$co_entidade[1], " | ", escola()$ds_endereco[1]), class = "tile-subheadline"),
        # renderTable({ escola() |> dplyr::select(info, valor)}, colnames = FALSE, width = "100%"),
      )
    })

    output$school_area <- renderUI({
      req(escola())
      tagList(
        h4("Quadro de Áreas", class = "tile-headline"),
        renderTable({
          escola() |>
            dplyr::select(info, valor) |>
            dplyr::filter(info != "Tempo até o metrô (minutos)")
          }, colnames = FALSE, width = "100%"),
      )
    })

    output$school_capacity <- renderUI({
      req(escola())

      tagList(
        h4("Edição de Vagas", class = "tile-headline"),

        div(
          style = "margin: 5px; padding: 5px;",
          fluidRow(
            column(4, p("Etapa")),
            column(4, p("Vagas Atuais")),
            column(4, p("Vagas Novas"))
          ),
          fluidRow(
            column(4, p("Creche")),
            column(4, textOutput(ns("cre_vagas"))),
            # column(4, numericInput(ns("cre_vagas"), label = "",
            #                        value = escola()$qt_mat_inf_cre[1],
            #                        min=escola()$qt_mat_inf_cre[1],
            #                        max=escola()$qt_mat_inf_cre[1])),
            column(4, numericInput(ns("cre_novas"), label = "",
                                   value = state$edit_school$nova_mat_creche,
                                   min=0))
          ),
          fluidRow(
            column(4, p("Pré-escola")),
            column(4, textOutput(ns("pre_vagas"))),
            # column(4, numericInput(ns("pre_vagas"), label = "",
            #                        value = escola()$qt_mat_inf_pre[1],
            #                        min=escola()$qt_mat_inf_pre[1],
            #                        max=escola()$qt_mat_inf_pre[1])),
            column(4, numericInput(ns("pre_novas"), label = "",
                                   value = state$edit_school$nova_mat_pre,
                                   min=0))
          ),
          fluidRow(
            column(4, p("Fundamental I")),
            column(4, textOutput(ns("fai_vagas"))),
            # column(4, numericInput(ns("fai_vagas"), label = "",
            #                        value = escola()$qt_mat_fund_ai[1],
            #                        min=escola()$qt_mat_fund_ai[1],
            #                        max=escola()$qt_mat_fund_ai[1])),
            column(4, numericInput(ns("fai_novas"), label = "",
                                   value = state$edit_school$nova_mat_fund_ai,
                                   min=0))
          ),
          fluidRow(
            column(4, p("Fundamental II")),
            column(4, textOutput(ns("faf_vagas"))),
            # column(4, numericInput(ns("faf_vagas"), label = "",
            #                        value = escola()$qt_mat_fund_af[1],
            #                        min=escola()$qt_mat_fund_af[1],
            #                        max=escola()$qt_mat_fund_af[1])),
            column(4, numericInput(ns("faf_novas"), label = "",
                                   value = state$edit_school$nova_mat_fund_af,
                                   min=0))
          )
        )
      )

    })


# Events ------------------------------------------------------------------

    output$cre_vagas <- renderText({ escola()$qt_mat_inf_cre[1] })
    output$pre_vagas <- renderText({ escola()$qt_mat_inf_pre[1] })
    output$fai_vagas <- renderText({ escola()$qt_mat_fund_ai[1] })
    output$faf_vagas <- renderText({ escola()$qt_mat_fund_af[1] })
    ## evitar que vagas atuais sejam alteradas
    # observeEvent(input$cre_vagas, {
    #   req(escola())
    #   isolate(updateNumericInput(session, "cre_vagas", value = escola()$qt_mat_inf_cre[1]))
    # })
    # observeEvent(input$pre_vagas, {
    #   req(escola())
    #   isolate(updateNumericInput(session, "pre_vagas", value = escola()$qt_mat_inf_pre[1]))
    # })
    # observeEvent(input$fai_vagas, {
    #   req(escola())
    #   isolate(updateNumericInput(session, "fai_vagas", value = escola()$qt_mat_fund_ai[1]))
    # })
    # observeEvent(input$faf_vagas, {
    #   req(escola())
    #   isolate(updateNumericInput(session, "faf_vagas", value = escola()$qt_mat_fund_af[1]))
    # })

    # salvar alterações nas vagas
    observeEvent(input$cre_novas, {
      state$edit_school$nova_mat_creche <- input$cre_novas
    })
    observeEvent(input$pre_novas, {
      state$edit_school$nova_mat_pre <- input$pre_novas
    })
    observeEvent(input$fai_novas, {
      state$edit_school$nova_mat_fund_ai <- input$fai_novas
    })
    observeEvent(input$faf_novas, {
      state$edit_school$nova_mat_fund_af <- input$faf_novas
    })


  })
}


