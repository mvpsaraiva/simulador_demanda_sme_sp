#' simulacao
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

create_new_scenario <- function(data) {

  # buscar matrículas das escolas modificadas no cenário
  escolas_df <- dplyr::left_join(data$escolas, data$escolas_mod, by = "co_entidade") |>
    dplyr::mutate(qt_mat_inf_cre = dplyr::if_else(is.na(nova_mat_creche), qt_mat_inf_cre, as.integer(nova_mat_creche)),
                  qt_mat_inf_pre = dplyr::if_else(is.na(nova_mat_pre), qt_mat_inf_pre, as.integer(nova_mat_pre)),
                  qt_mat_fund_ai = dplyr::if_else(is.na(nova_mat_fund_ai), qt_mat_fund_ai, as.integer(nova_mat_fund_ai)),
                  qt_mat_fund_af = dplyr::if_else(is.na(nova_mat_fund_af), qt_mat_fund_af, as.integer(nova_mat_fund_af))
                  )

  # recalcular matrículas por hexágono
  matriculas_por_hex <- calcular_matriculas_por_hex(escolas_df)

  # calcular bfca
  bfca_tempos_viagem <- c(15, 30)
  bfca_anos <- c(2020, 2035, 2045)
  bfca_series <- c("creche", "pre", "anos_iniciais", "anos_finais")

  combinacoes <- purrr::cross3(bfca_tempos_viagem, bfca_anos, bfca_series)

  bfca_df <- purrr::map_df(combinacoes, function(params) {
    bfca <- calcular_bfca(hexgrid = data$hexgrid,
                          demanda_por_hex = data$populacao,
                          matriculas_por_hex = matriculas_por_hex,
                          travel_times = data$ttm,
                          tempo_maximo_viagem = params[[1]],
                          ano_previsao = params[[2]],
                          serie_ensino = params[[3]] )

    return(bfca)
  })

  # calcular deficit de vagas com base no bfca

  deficit_bfca_hex <- calcular_deficit_bfca_hex(bfca_df)
  deficit_bfca_setor <- calcular_deficit_bfca_setor(bfca_df)
  deficit_bfca_distrito <- calcular_deficit_bfca_distrito(bfca_df)

  data$deficit_hex <- deficit_bfca_hex
  data$deficit_setor <- deficit_bfca_setor
  data$deficit_distrito <- deficit_bfca_distrito

  return(data)
}

persist_scenario <- function(db_con, data) {

  # dados a serem salvos no cenário:
  # cabeçalho
  # escolas modificadas
  # déficits por hexagono, setor e distrito

  # primeiro, testar se a tabela de cenários existe
  # se não existe, o id do primeiro cenário é 1
  # se existe, buscar o id máximo e adicionar 1

  new_id <- 0
  if (DBI::dbExistsTable(db_con, "cenarios")) {
    cenarios <- DBI::dbGetQuery(db_con, "SELECT max(id) AS id FROM cenarios")
    new_id <- cenarios$id[1] + 1
  } else {
    new_id <- 1
  }

  # tabela com o cabeçalho do cenário
  scenario_df <- data.frame(
    id = new_id,
    data = data$data_criacao,
    nome = data$nome,
    autor = data$autor,
    descricao = data$descricao
  )

  # grava o novo cenário no banco de dados para reservar o id o mais rápido possível
  DBI::dbWriteTable(db_con, name = "cenarios", value = scenario_df, append = TRUE)

  # grava o restante dos dados
  # utilizar uma transação, pois são dados mais pesados
  # inicia transacao
  DBI::dbBegin(db_con)

  # escolas modificadas
  if (nrow(data$escolas_mod) > 0) {
    data$escolas_mod$id_cenario <- new_id
    data$escolas_mod <- dplyr::select(data$escolas_mod, id_cenario, dplyr::everything())

    DBI::dbWriteTable(db_con, name = "modificacoes", value = data$escolas_mod, append = TRUE)
  }

  # déficit por hexágono
  data$deficit_hex$id_cenario <- new_id
  data$deficit_hex <- dplyr::select(data$deficit_hex, id_cenario, dplyr::everything())
  DBI::dbWriteTable(db_con, name = "deficit_por_hex", value = data$deficit_hex, append = TRUE)

  data$deficit_setor$id_cenario <- new_id
  data$deficit_setor <- dplyr::select(data$deficit_setor, id_cenario, dplyr::everything())
  DBI::dbWriteTable(db_con, name = "deficit_por_setor", value = data$deficit_setor, append = TRUE)

  data$deficit_distrito$id_cenario <- new_id
  data$deficit_distrito <- dplyr::select(data$deficit_distrito, id_cenario, dplyr::everything())
  DBI::dbWriteTable(db_con, name = "deficit_por_distrito", value = data$deficit_distrito, append = TRUE)

  DBI::dbCommit(db_con)

}


calcular_matriculas_por_hex <- function(escolas) {

  # calcula matrículas por hexágono
  matriculas <- escolas |>
    dplyr::group_by(id_hex, tp_categoria) |>
    dplyr::summarise(across(c(qt_mat_inf_cre, qt_mat_inf_pre, qt_mat_fund_ai, qt_mat_fund_af), sum), .groups = "drop") |>
    tidyr::pivot_longer(cols = starts_with("qt_mat"), names_to = "serie", values_to = "matriculas")

  # organiza campos categóricos
  matriculas$faixa_idade <- dplyr::recode(matriculas$serie,
                                   qt_mat_inf_cre = "de_00_a_03_anos",
                                   qt_mat_inf_pre = "de_04_a_05_anos",
                                   qt_mat_fund_ai = "de_06_a_10_anos",
                                   qt_mat_fund_af = "de_11_a_14_anos"
  )

  matriculas$etapa <- dplyr::recode(matriculas$serie,
                             qt_mat_inf_cre = "infantil",
                             qt_mat_inf_pre = "infantil",
                             qt_mat_fund_ai = "fundamental",
                             qt_mat_fund_af = "fundamental"
  )

  matriculas$serie <- dplyr::recode(matriculas$serie,
                             qt_mat_inf_cre = "creche",
                             qt_mat_inf_pre = "pre",
                             qt_mat_fund_ai = "anos_iniciais",
                             qt_mat_fund_af = "anos_finais"
  )

  # prepara dataset final
  matriculas_clean <- matriculas |>
    dplyr::select(id_hex, tp_categoria,
           faixa_idade, etapa, serie, matriculas) |>
    tidyr::pivot_wider(names_from = tp_categoria, values_from = matriculas,
                values_fill = 0, names_prefix = "mat_") |>
    janitor::clean_names()

  matriculas_df <- matriculas_clean |>
    dplyr::mutate(mat_publica = mat_municipal + mat_estadual,
           mat_total = mat_privada + mat_municipal + mat_estadual) |>
    dplyr::filter(mat_total > 0)

  return(matriculas_df)
}


calcular_bfca <- function(hexgrid, demanda_por_hex, matriculas_por_hex, travel_times,
                          tempo_maximo_viagem, ano_previsao, serie_ensino) {

  # filtrar dados relevantes
  demanda <- demanda_por_hex |>
    dplyr::filter(ano == ano_previsao, serie == serie_ensino)

  matriculas = matriculas_por_hex |> dplyr::filter(serie == serie_ensino)

  # unir tabelas de demanda e oferta
  landuse_df <- dplyr::full_join(demanda, matriculas,  by = c("id_hex", "faixa_idade", "etapa", "serie")) |>
    dplyr::rename(id = id_hex) |>
    tidyr::replace_na(replace = list(mat_municipal = 0, mat_privada = 0,
                              mat_estadual = 0, mat_publica = 0,
                              mat_total = 0,
                              populacao_distrito = 0, populacao_serie_distrito = 0,
                              populacao_total_hex = 0, populacao_serie_hex = 0,
                              populacao_esc_publica_hex = 0,
                              ano = ano_previsao)) |>
    tidyr::drop_na()

  landuse_df <- landuse_df |>
    dplyr::filter(populacao_esc_publica_hex + mat_publica > 0)

  access_bfca_df <-
    accessibility::floating_catchment_area(travel_matrix = travel_times,
                                           land_use_data = landuse_df,
                                           opportunity = "mat_publica",
                                           travel_cost = "travel_time",
                                           demand = "populacao_esc_publica_hex",
                                           method = "bfca",
                                           decay_function = accessibility::decay_binary(cutoff = tempo_maximo_viagem))


  # preparação do dataset de saída

  ## parâmetros da análise
  params_df <- demanda |> dplyr::select(ano, faixa_idade, etapa, serie) |>
    dplyr::slice(1)

  ## junção do hexgrid completo com os resultados da acessibilidade
  access_hex <- hexgrid |>
    sf::st_set_geometry(NULL) |>
    dplyr::left_join(access_bfca_df, by = c("id_hex" = "id")) |>
    dplyr::rename(bfca = mat_publica) |>
    cbind(params_df) |>
    dplyr::mutate(cd_distrito = as.numeric(cd_distrito), cutoff = tempo_maximo_viagem)

  access_hex_expandida <- access_hex |>
    dplyr::left_join(demanda, by = c("id_hex", "ano", "faixa_idade", "etapa", "serie")) |>
    dplyr::left_join(matriculas, by = c("id_hex", "faixa_idade", "etapa", "serie"))


  ## seleção das colunas relevantes
  access_hex <- access_hex_expandida |>
    dplyr::select(id_hex, cd_distrito, nr_distrito, nm_distrito,
           cd_dre, cd_setor, ano, faixa_idade, etapa, serie,
           populacao = populacao_esc_publica_hex, matriculas = mat_publica,
           cutoff, bfca) |>
    tidyr::replace_na(replace = list(populacao = 0, matriculas = 0))

  return(access_hex)
}


calcular_deficit_bfca_hex <- function(bfca) {

  # transformar BFCA em medida de déficit / superávit
  def_bfca <- bfca |>
    dplyr::mutate(vagas_acessiveis = round(populacao * bfca),
           deficit = vagas_acessiveis - round(populacao)) |>
    dplyr::mutate(populacao = round(populacao))

  return(def_bfca)

}

# bfca <- tar_read(acessibilidade_bfca)
calcular_deficit_bfca_setor <- function(bfca) {

  # transformar BFCA em medida de déficit / superávit
  def_bfca_setor <- bfca |>
    dplyr::group_by(cd_dre, cd_distrito, nr_distrito, nm_distrito, cd_setor,
             ano, faixa_idade, etapa, serie, cutoff) |>
    dplyr::mutate(vagas_acessiveis = round(populacao * bfca),
           populacao = round(populacao),
           deficit = vagas_acessiveis - populacao) |>
    dplyr::summarise(populacao = sum(populacao), matriculas = sum(matriculas),
              vagas_acessiveis = sum(vagas_acessiveis),
              deficit = sum(deficit, na.rm = TRUE), .groups = "drop")


  return(def_bfca_setor)

}

# bfca <- tar_read(acessibilidade_bfca)
calcular_deficit_bfca_distrito <- function(bfca) {

  # transformar BFCA em medida de déficit / superávit
  def_bfca_distrito <- bfca |>
    dplyr::group_by(cd_dre, cd_distrito, nr_distrito, nm_distrito,
             ano, faixa_idade, etapa, serie, cutoff) |>
    dplyr::mutate(vagas_acessiveis = round(populacao * bfca),
           populacao = round(populacao),
           deficit = vagas_acessiveis - populacao) |>
    dplyr::summarise(populacao = sum(populacao), matriculas = sum(matriculas),
              vagas_acessiveis = sum(vagas_acessiveis),
              deficit = sum(deficit, na.rm = TRUE), .groups = "drop")


  return(def_bfca_distrito)
}
