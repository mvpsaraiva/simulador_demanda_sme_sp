## code to prepare `tmi_metro` dataset goes here

library(tidyverse)

hexes <- metro |>
  select(lon, lat) |>
  h3jsr::point_to_cell(res = 9)

metro$id_hex <- hexes

tmi_metro <- ttm |>
  inner_join(metro, by = c("to_id" = "id_hex")) |>
  filter(modo == "Metrô") |>
  group_by(from_id) |>
  arrange(travel_time) |>
  slice(1) |>
  ungroup() |>
  select(id_hex = to_id, travel_time)

usethis::use_data(tmi_metro, overwrite = TRUE)

