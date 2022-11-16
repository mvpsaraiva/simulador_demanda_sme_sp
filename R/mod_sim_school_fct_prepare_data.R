prepare_school_data <- function(escolas_df, codigo) {
  # preparar dados das escolas
  escola <- escolas_df |>
    dplyr::filter(co_entidade == codigo) |>
    tidyr::pivot_longer(cols = c(qt_area_edificada,
                                 qt_area_livre_terreno,
                                 qt_area_ocupada_terreno,
                                 qt_area_total_terreno,
                                 qt_pavimentos,
                                 qt_salas,
                                 qt_salas_utilizadas,
                                 tmi_metro),
                        names_to = "info",
                        values_to = "valor")

  escola$info <- factor(
    escola$info,
    levels = c(
      "qt_area_edificada",
      "qt_area_livre_terreno",
      "qt_area_ocupada_terreno",
      "qt_area_total_terreno",
      "qt_pavimentos",
      "qt_salas",
      "qt_salas_utilizadas",
      "tmi_metro"
    ),
    labels = c(
      "Área edificada",
      "Área livre no terreno",
      "Área ocupada no terreno",
      "Área total do terreno",
      "Número de pavimentos",
      "Número de salas",
      "Número de salas utilizadas",
      "Tempo até o metrô (minutos)"
    )
  )

  return(escola)
}

prepare_sector_deficit_data <- function(escolas_df, codigo) {
  setor <- escolas_df |>
    dplyr::filter(co_entidade == codigo) |>
    dplyr::select(cd_setor) |>
    dplyr::pull()


  deficit <- deficit_bfca_setor |>
    dplyr::filter(cd_setor == setor, cutoff == 15) |>
    dplyr::select(ano, serie, deficit) |>
    tidyr::pivot_wider(names_from = ano, values_from = deficit)

  return(deficit)

}

prepare_district_deficit_data <- function(escolas_df, codigo) {
  distrito <- escolas_df |>
    dplyr::filter(co_entidade == codigo) |>
    dplyr::select(nr_distrito) |>
    dplyr::pull()


  deficit <- deficit_bfca_distrito |>
    dplyr::filter(nr_distrito == distrito, cutoff == 15) |>
    dplyr::select(ano, serie, deficit) |>
    tidyr::pivot_wider(names_from = ano, values_from = deficit)

  return(deficit)

}
