## code to prepare `deficit_bfca_hex` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

deficit_bfca_hex <- DBI::dbReadTable(db_con, "deficit_bfca_hex") |>
  dplyr::filter(cutoff == 15) |>
  dplyr::mutate(superavit = purrr::map2_dbl(deficit, 0, max),
                deficit = purrr::map2_dbl(deficit, 0, min)) |>
  dplyr::ungroup()

usethis::use_data(deficit_bfca_hex, overwrite = TRUE)


db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_simulations.db")
DBI::dbListTables(db_con)

deficit_bfca_hex <- DBI::dbReadTable(db_con, "deficit_por_hex") |>
  dplyr::filter(cutoff == 15, id_cenario == 3) |>
  dplyr::mutate(superavit = purrr::map2_dbl(deficit, 0, max),
                deficit = purrr::map2_dbl(deficit, 0, min)) |>
  dplyr::ungroup()


df <- DBI::dbReadTable(db_con, "adicoes")

df |>
  filter(id_cenario == 16) |>
  left_join(hexgrid)

