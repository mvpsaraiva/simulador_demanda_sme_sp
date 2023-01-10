#' utils_simulacao
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

novo_escolas_mod_vazio <- function() {
  escolas = data.frame(co_entidade = numeric(),
                       no_entidade = character(),
                       orig_mat_creche = numeric(),
                       nova_mat_creche = numeric(),

                       orig_mat_pre = numeric(),
                       nova_mat_pre = numeric(),

                       orig_mat_fund_ai = numeric(),
                       nova_mat_fund_ai = numeric(),

                       orig_mat_fund_af = numeric(),
                       nova_mat_fund_af = numeric()
  )

  return(escolas)
}

# connection <- db_con
carrega_novas_escolas <- function(connection) {
  if (!DBI::dbExistsTable(connection, "novas_escolas")) {
    novas_escolas <- escolas[NULL, ]

    DBI::dbWriteTable(connection, name = "novas_escolas", value = novas_escolas)
  }

  if (!DBI::dbExistsTable(connection, "adicoes")) {
    adicoes <- escolas[NULL, ]

    DBI::dbWriteTable(connection, name = "adicoes", value = adicoes)
  }

  novas_escolas <- DBI::dbReadTable(connection, "novas_escolas") |>
    # algumas escolas foram salvas com id hex maiusculo... corrigir
    dplyr::mutate(id_hex = tolower(id_hex))
  return(novas_escolas)
}

adiciona_escola <- function(connection, df) {

  novas_escolas <- DBI::dbReadTable(connection, "novas_escolas") |>
    # algumas escolas foram salvas com id hex maiusculo... corrigir
    dplyr::mutate(id_hex = tolower(id_hex))

  new_id = max(novas_escolas$co_entidade, na.rm = TRUE) + 1
  if (is.infinite(new_id)) new_id = 1
  df$co_entidade = new_id

  DBI::dbWriteTable(connection, name = "novas_escolas", value = df, append = TRUE)

  return(DBI::dbReadTable(connection, "novas_escolas"))

}

edita_escola <- function(connection, df) {
  # remove versão anterior da escola
  id <- df$co_entidade[1]
  DBI::dbSendQuery(connection, paste0("delete from novas_escolas where co_entidade = ", id))

  # salva nova versão da escola
  DBI::dbWriteTable(connection, name = "novas_escolas", value = df, append = TRUE)

  #retorna dataframe completo
  return(DBI::dbReadTable(connection, "novas_escolas"))

}

remove_escolas <- function(connection, ids) {
  purrr::walk(ids, function(id){
    DBI::dbSendQuery(connection, paste0("delete from novas_escolas where co_entidade = ", id))
  })

  return(DBI::dbReadTable(connection, "novas_escolas"))
}

