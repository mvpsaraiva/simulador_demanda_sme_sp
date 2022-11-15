#' sim_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_table_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' sim_table Server Functions
#'
#' @noRd
mod_sim_table_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Tabela de escolas -------------------------------------------------------
    escolas <- reactiveValues(data = DBI::dbReadTable(state$db_con, "escolas"))
    tmi <- reactiveValues(data = DBI::dbReadTable(state$db_con, "tmi_metro_access"))

    tbl_escolas <- reactive({
      escolas_df <-
        escolas$data |> dplyr::filter(tp_categoria != "privada")
      setores_df <-
        DBI::dbReadTable(state$db_con, "setores_sme") |> dplyr::select(-geometry)
      distritos_df <-
        DBI::dbReadTable(state$db_con, "distritos") |> dplyr::select(-geometry)

      escolas_joined_df <-
        dplyr::left_join(escolas_df,
                         setores_df,
                         by = c("co_setor" = "cd_setor", "cd_distrito")) |>
        dplyr::left_join(distritos_df,
                         by = c("cd_distrito", "cd_muni", "nr_distrito", "cd_dre"))

      escolas_clean_df <-
        dplyr::select(
          escolas_joined_df,
          cd_dre,
          nr_distrito,
          label_distrito,
          co_setor,
          co_entidade,
          no_entidade,
          tp_categoria,
          qt_mat_inf_cre,
          qt_mat_inf_pre,
          qt_mat_fund_ai,
          qt_mat_fund_af
        ) |>
        dplyr::mutate(cd_dre = stringr::str_remove(cd_dre, "DRE - ")) |>
        dplyr::arrange(co_setor, no_entidade)

      return(escolas_clean_df)
    })

  })
}

## To be copied in the UI
# mod_sim_table_ui("sim_table_1")

## To be copied in the server
# mod_sim_table_server("sim_table_1")
