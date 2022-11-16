#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(
      tags$script(HTML('
        $(window).resize(function(event){
          var w = $(this).width();
          var h = $(this).height();
          var obj = {width: w, height: h};
          Shiny.onInputChange("window_size", obj);
        });
      '))),
    # Your application UI logic
    navbarPage(
      title = "demanda.sme.sp",
      collapsible = TRUE,
      # tabPanel(
      #   title = "Estudantes",
      #   mod_estudantes_ui("estudantes_1")
      # ),
      # tabPanel(
      #   title = "Matrículas",
      #   mod_matriculas_ui("matriculas_1")
      #   ),
      # tabPanel(
      #   title = "Déficit",
      #   mod_deficit_ui("deficit_1")
      # ),
      tabPanel(
        title = "Simulação",
        style = "margin: 0; padding: 0; height: 100%",
        mod_simulation_ui("simulation"),
      )
      # tabPanel(
      #   title = "Resultados",
      #   mod_resultados_ui("resultados_1")
      # )
    ))
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "demanda.sme.sp")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
