## code to prepare `deficit_bfca_distrito` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

deficit_bfca_distrito <- DBI::dbReadTable(db_con, "deficit_bfca_distrito")

usethis::use_data(deficit_bfca_distrito, overwrite = TRUE)
