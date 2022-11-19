## code to prepare `populacao_por_hex` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

populacao_por_hex <- DBI::dbReadTable(db_con, "populacao_por_hex") |>
  dplyr::filter(ano %in% c(2020, 2035, 2045))

usethis::use_data(populacao_por_hex, overwrite = TRUE)
