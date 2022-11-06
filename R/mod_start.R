#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_ui <- function(id){
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c(
        "     200px   1fr   ",
        "85px header  header",
        "1fr  sidebar plot  "
      ),
      gridlayout::grid_card_text("header", "Geysers!", is_title = TRUE),
      gridlayout::grid_card(
        "sidebar",
        title = "Settings",
        sliderInput("bins","Number of bins:",
                    min = 1, max = 50, value = 30, width = "100%")
      ),
      gridlayout::grid_card(
        "plot",
        plotOutput("distPlot", height="100%")
      )
    )

  )
}

#' start Server Functions
#'
#' @noRd
mod_start_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$distPlot <- renderPlot({
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
  })
}

## To be copied in the UI
# mod_start_ui("start_1")

## To be copied in the server
# mod_start_server("start_1")
