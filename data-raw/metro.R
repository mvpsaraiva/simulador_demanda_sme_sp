## code to prepare `metro` dataset goes here
metro <- read.csv("../data_geo/estacoes_alta_capacidade.csv") |>
  janitor::clean_names() |>
  dplyr::filter(cidade_n == "São Paulo", situa_o == "Operacional") |>
  dplyr::select(modo, corredor, estacao = esta_o, tipo, situacao = situa_o, tma, lon = x, lat = y)

mapview::mapview(metro, xcol="lon", ycol="lat", zcol="situacao", crs=4326)

usethis::use_data(metro, overwrite = TRUE)

