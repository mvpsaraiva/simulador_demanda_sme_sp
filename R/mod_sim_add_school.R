#' sim_add_school UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_add_school_ui <- function(id){
  ns <- NS(id)
  tagList(

    gridlayout::grid_container(
      layout = c("       400px ",
                 "600px  data  "
      ),
      gridlayout::grid_card(
        "data",
        p(),
        h4("Adicionar Escola", class = "tile-headline"),
        # textInput(inputId = ns("co_entidade"),
        #           label = "Código",
        #           value = "",
        #           placeholder = "0",
        #           width = "100%"),
        textInput(inputId = ns("no_entidade"),
                  label = "Nome",
                  placeholder = "Nome da Escola",
                  width = "100%"),
        textInput(inputId = ns("cd_setor"),
                  label = "Setor",
                  placeholder = "Setor",
                  width="100%"),
        textInput(inputId = ns("nm_distrito"),
                  label = "Distrito",
                  placeholder = "Distrito",
                  width="100%"),
        textInput(inputId = ns("nm_dre"),
                  label = "DRE",
                  placeholder = "DRE",
                  width="100%"),
        hr(),
        h4("Número de Vagas", class = "tile-headline"),
        p("* Capacidade sugerida de acordo com o déficit de vagas no entorno da nova escola, considerando 15 minutos de caminhada."),
        splitLayout(cellWidths = c("25%", "75%"),
          textOutput(outputId = ns("lbl_creche")),
          numericInput(inputId = ns("qt_mat_inf_cre"),
                       label = "Vagas de Creche",
                       value = 0,
                       width="100%")
        ),
        splitLayout(cellWidths = c("25%", "75%"),
          textOutput(outputId = ns("lbl_pre")),
          numericInput(inputId = ns("qt_mat_inf_pre"),
                       label = "Vagas de Creche",
                       value = 0,
                       width="100%")
        ),
        splitLayout(cellWidths = c("25%", "75%"),
          textOutput(outputId = ns("lbl_fund_ai")),
          numericInput(inputId = ns("qt_mat_fund_ai"),
                       label = "Vagas de Creche",
                       value = 0,
                       width="100%")
        ),
        splitLayout(cellWidths = c("25%", "75%"),
          textOutput(outputId = ns("lbl_fund_af")),
          numericInput(inputId = ns("qt_mat_fund_af"),
                       label = "Vagas de Creche",
                       value = 0,
                       width="100%")
        )
      )
    )

  )
}

#' sim_add_school Server Functions
#'
#' @noRd
mod_sim_add_school_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$lbl_creche <- renderText("Creche")
    output$lbl_pre <- renderText("Pré-escola")
    output$lbl_fund_ai <- renderText("Fundamental I")
    output$lbl_fund_af <- renderText("Fundamental II")

    observeEvent(state$new_school, {

      isolate(updateTextInput(session, "no_entidade", value = state$add_school$no_entidade))

      isolate(updateTextInput(session, "cd_setor", value = state$add_school$cd_setor))
      isolate(updateTextInput(session, "nm_distrito", value = state$add_school$nm_distrito))
      isolate(updateTextInput(session, "nm_dre", value = state$add_school$nm_dre))

      isolate(updateNumericInput(session, "qt_mat_inf_cre", value = isolate(state$add_school$qt_mat_inf_cre)))
      isolate(updateNumericInput(session, "qt_mat_inf_pre", value = isolate(state$add_school$qt_mat_inf_pre)))
      isolate(updateNumericInput(session, "qt_mat_fund_ai", value = isolate(state$add_school$qt_mat_fund_ai)))
      isolate(updateNumericInput(session, "qt_mat_fund_af", value = isolate(state$add_school$qt_mat_fund_af)))

      shinyjs::disable("cd_setor")
      shinyjs::disable("nm_distrito")
      shinyjs::disable("nm_dre")

    })

    observeEvent(input$no_entidade, { state$add_school$no_entidade = isolate({input$no_entidade}) })

    observeEvent(input$qt_mat_inf_cre, {
      state$add_school$qt_mat_inf_cre = isolate({input$qt_mat_inf_cre})
    })
    observeEvent(input$qt_mat_inf_pre, {
      state$add_school$qt_mat_inf_pre = isolate({input$qt_mat_inf_pre})
    })
    observeEvent(input$qt_mat_fund_ai, {
      state$add_school$qt_mat_fund_ai = isolate({input$qt_mat_fund_ai})
    })
    observeEvent(input$qt_mat_fund_af, {
      state$add_school$qt_mat_fund_af = isolate({input$qt_mat_fund_af})
    })


  })
}

## To be copied in the UI
# mod_sim_add_school_ui("sim_add_school_1")

## To be copied in the server
# mod_sim_add_school_server("sim_add_school_1")
