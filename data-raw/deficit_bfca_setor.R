## code to prepare `deficit_bfca_setor` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

deficit_bfca_setor <- DBI::dbReadTable(db_con, "deficit_bfca_setor")

usethis::use_data(deficit_bfca_setor, overwrite = TRUE)
