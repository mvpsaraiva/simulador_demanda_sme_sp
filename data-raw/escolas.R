## code to prepare `escolas` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

load("data/setores.rda")
tmi <- DBI::dbReadTable(db_con, "tmi_metro_access")

escolas <- DBI::dbReadTable(db_con, "escolas")

escolas_joined <- escolas |>
  dplyr::left_join(setores, by = c("co_setor" = "cd_setor")) |>
  dplyr::left_join(tmi, by = c("id_hex" = "id"))


escolas <-
  escolas_joined |>
  dplyr::mutate(ds_endereco = paste0(ds_complemento, " ", ds_endereco, ", ", nu_endereco)) |>
  dplyr::select(
    co_entidade,
    no_entidade,
    tp_categoria,

    ds_endereco,
    cd_setor = co_setor,
    nr_distrito,
    nm_distrito,
    cd_dre,
    nm_dre,
    id_hex,
    lat,
    lon,

    qt_mat_inf_cre,
    qt_mat_inf_pre,
    qt_mat_fund_ai,
    qt_mat_fund_af,

    qt_area_edificada,
    qt_area_livre_terreno = qt_area_livre,
    qt_area_ocupada_terreno = qt_area_ocupada,
    qt_area_total_terreno = qt_area_total,
    qt_pavimentos = qt_pavimento,
    qt_salas,
    qt_salas_utilizadas = qt_salas_utilizadas_dentro,
    tmi_metro = travel_time
  ) |>
  dplyr::arrange(cd_setor, no_entidade)

escolas$tp_categoria <- factor(escolas$tp_categoria,
                               levels = c("privada", "municipal", "estadual"),
                               labels = c("Conveniada", "Municipal", "Estadual"))


usethis::use_data(escolas, overwrite = TRUE)


