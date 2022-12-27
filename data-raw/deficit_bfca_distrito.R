## code to prepare `deficit_bfca_distrito` dataset goes here
db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v4.db")
DBI::dbListTables(db_con)

library(dplyr)

deficit_bfca_distrito <- deficit_bfca_setor |>
  group_by(cd_dre, nr_distrito, nm_distrito,
           ano, faixa_idade, etapa, serie, cutoff) |>
  summarise(populacao = sum(populacao), matriculas = sum(matriculas),
            vagas_acessiveis = sum(vagas_acessiveis),
            deficit = sum(deficit), superavit = sum(superavit), .groups = "drop")


# deficit_bfca_distrito <- DBI::dbReadTable(db_con, "deficit_bfca_distrito")

usethis::use_data(deficit_bfca_distrito, overwrite = TRUE)
