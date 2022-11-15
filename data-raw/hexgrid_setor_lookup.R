## code to prepare `hexgrid_setor_lookup` dataset goes here

db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

hexgrid <- read_sf_from_db(db_con, "hexgrid") |> sf::st_set_geometry(NULL)

hexgrid_setor_lookup <- hexgrid |> dplyr::select(cd_setor, id_hex) |>
  dplyr::arrange(cd_setor, id_hex) |>
  dplyr::distinct()

usethis::use_data(hexgrid_setor_lookup, overwrite = TRUE)

DBI::dbDisconnect(db_con)
