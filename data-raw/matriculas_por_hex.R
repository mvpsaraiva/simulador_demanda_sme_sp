## code to prepare `matriculas_por_hex` dataset goes here
## code to prepare `populacao_por_hex` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

matriculas_por_hex <- DBI::dbReadTable(db_con, "matriculas_por_hex") |>
  dplyr::select(-mat_total, -mat_privada) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("mat"), names_to = "rede", values_to = "matriculas") |>
  dplyr::mutate(rede = stringr::str_remove(rede, "mat_"))

usethis::use_data(matriculas_por_hex, overwrite = TRUE)
