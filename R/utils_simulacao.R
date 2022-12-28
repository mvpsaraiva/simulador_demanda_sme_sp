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

  novas_escolas <- DBI::dbReadTable(connection, "novas_escolas")
  return(novas_escolas)
}
