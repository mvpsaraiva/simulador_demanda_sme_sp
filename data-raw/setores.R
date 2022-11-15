## code to prepare `setores` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

setores <- read_sf_from_db(db_con, "setores_sme")
distritos <- read_sf_from_db(db_con, "distritos") |> sf::st_set_geometry(NULL)

setores_j <- dplyr::left_join(setores, distritos, by = c("cd_distrito"))

setores <- setores_j |>
  dplyr::select(cd_setor, nr_distrito = nr_distrito.x, nm_distrito = label_distrito,
                cd_dre = cd_dre.x, nm_dre, geometry)

usethis::use_data(setores, overwrite = TRUE)

DBI::dbDisconnect(db_con)
