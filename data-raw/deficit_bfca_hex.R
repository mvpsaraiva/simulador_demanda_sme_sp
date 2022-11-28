## code to prepare `deficit_bfca_hex` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

deficit_bfca_hex <- DBI::dbReadTable(db_con, "deficit_bfca_hex") |>
  dplyr::filter(cutoff == 15)

usethis::use_data(deficit_bfca_hex, overwrite = TRUE)
