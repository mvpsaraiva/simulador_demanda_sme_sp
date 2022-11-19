## code to prepare `ttm` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

ttm <- DBI::dbReadTable(db_con, "travel_times")

usethis::use_data(ttm, overwrite = TRUE)

