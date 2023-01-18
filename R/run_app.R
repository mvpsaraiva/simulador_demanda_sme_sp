#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  db_path = "data/demanda_sme_simulations.db",
  mapbox_token = "pk.eyJ1IjoibXZwc2FyYWl2YSIsImEiOiJjbGFpaG9kdmMwMnE5M29wOHV2MGM4Y3B2In0.sl-zkob04cCpUiu0TxneTg",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui, enable_admin = TRUE, language = "pt-BR"),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      db_path = db_path,
      mapbox_token = mapbox_token,
      ...)
  )
}
