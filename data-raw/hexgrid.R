## code to prepare `hexgrid` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

hexgrid <- read_sf_from_db(db_con, "hexgrid")
hexgrid <- hexgrid |> dplyr::select(id_hex, geometry)

usethis::use_data(hexgrid, overwrite = TRUE)

DBI::dbDisconnect(db_con)
