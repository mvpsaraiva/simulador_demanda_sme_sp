## code to prepare `deficit_bfca_setor` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

library(dplyr)

deficit_bfca_setor <- left_join(deficit_bfca_hex, hexgrid_setor_lookup) |>
  left_join(sf::st_set_geometry(setores, NULL)) |>
  group_by(cd_dre, nr_distrito, nm_distrito, cd_setor,
           ano, faixa_idade, etapa, serie, cutoff) |>
  summarise(populacao = sum(populacao), matriculas = sum(matriculas),
            vagas_acessiveis = sum(vagas_acessiveis),
            deficit = sum(deficit), superavit = sum(superavit), .groups = "drop")


# deficit_bfca_setor <- DBI::dbReadTable(db_con, "deficit_bfca_setor")

usethis::use_data(deficit_bfca_setor, overwrite = TRUE)
