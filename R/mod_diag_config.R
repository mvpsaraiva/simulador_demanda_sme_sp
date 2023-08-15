#' diag_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_diag_config_ui <- function(id){
  ns <- NS(id)
  tagList(
    # style = "margin: 5px; padding: 5px;",
    div(
      style = "margins: 5px; padding: 5px;",
      h4("Configuração", class = "tile-headline"),
      hr(),

      h4("Filtro", class = "tile-headline"),
      # DRE
      shinyWidgets::pickerInput(ns("filtro_dre"),
                                label = "DREs",
                                multiple = TRUE,
                                choices = NULL,
                                inline = TRUE,
                                width = "300px"),

      # Distrito
      shinyWidgets::pickerInput(ns("filtro_distrito"),
                                label = "Distritos",
                                multiple = TRUE,
                                choices = NULL,
                                inline = TRUE,
                                width = "300px"),

      h4("Unidade Espacial", class = "tile-headline"),

      # unidades espaciais
      shinyWidgets::pickerInput(ns("unidade_espacial"),
                                label = "Divisão Espacial",
                                multiple = FALSE,
                                choices = list("Hexágonos" = "hexgrid",
                                               "Setores" = "setores_sme",
                                               "Nenhuma" = "nenhuma"),
                                # "Distritos" = "distritos"
                                selected = "setores_sme",
                                inline = TRUE,
                                width = "300px"),

      h4("Configurações do Mapa", class = "tile-headline"),

      # variável
      shinyWidgets::pickerInput(ns("mapa_variavel"),
                                label = "Mapear",
                                multiple = FALSE,
                                choices = c("Demanda (estudantes)" = "demanda",
                                            "Oferta (vagas)" = "oferta",
                                            "Déficit de vagas" = "deficit",
                                            "Superávit de vagas" = "superavit"
                                ),
                                selected = "demanda",
                                inline = TRUE,
                                width = "300px"),
      # etapa de ensino
      shinyWidgets::pickerInput(ns("mapa_etapa"),
                                label = "Etapa de ensino",
                                multiple = TRUE,
                                choices = c("Creche" = "creche",
                                            "Pré-Escola" = "pre",
                                            "Fundamental I" = "anos_iniciais",
                                            "Fundamental II" = "anos_finais"),
                                selected = c("creche", "pre", "anos_iniciais", "anos_finais"),
                                inline = TRUE,
                                width = "300px"),
      # rede de ensino
      shinyWidgets::pickerInput(ns("mapa_rede"),
                                label = "Rede de ensino",
                                multiple = FALSE,
                                choices = c("Pública" = "publica",
                                            "Municipal" = "municipal",
                                            "Estadual" = "estadual"),
                                selected = "publica",
                                inline = TRUE,
                                width = "200px"),
      # etapa de ensino
      shinyWidgets::pickerInput(ns("mapa_ano"),
                                label = "Ano da previsão",
                                multiple = FALSE,
                                choices = c("2035" = 2035,
                                            "2045" = 2045),
                                selected = "2020",
                                inline = TRUE,
                                width = "200px"
      ),
      # mapear escolas
      div(
        p("Exibir escolas"),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("mapa_exibir_escolas"),
          label = NULL, # "Exibir escolas",
          choices = c("Conveniadas" = "Conveniada",
                      "Municipais" = "Municipal",
                      "Estaduais" = "Estadual"),
          selected = NULL,
          individual = TRUE
        )
      ),
      div(
        p("Exibir estações de transporte público"),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("mapa_exibir_metro"),
          label = NULL, # "Exibir escolas",
          choices = c("Metrô",
                      "Monotrilho",
                      "BRT",
                      "Trem"),
          selected = NULL,
          individual = TRUE
        )
      )
    )
  )
}

#' diag_config Server Functions
#'
#' @noRd
mod_diag_config_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # DRE e Distritos para filtro
    observe({
      req(setores)

      # DRE
      dre_list <- setores$nm_dre |>
        unique() |>
        sort()
      shinyWidgets::updatePickerInput(session, "filtro_dre", choices = dre_list, selected = "")

      # Distritos
      district_list <- setores$nm_distrito |>
        unique() |>
        sort()
      shinyWidgets::updatePickerInput(session, "filtro_distrito", choices = district_list, selected = "")
    })

    observeEvent(input$filtro_dre, ignoreNULL = FALSE, {
      req(setores)

      if (!is.null(input$filtro_dre)) {
        state$config$selected_districts = NULL
        state$config$selected_dres = input$filtro_dre

        # Distritos da DRE
        district_list <- setores |>
          dplyr::filter(nm_dre %in% input$filtro_dre) |>
          dplyr::pull(nm_distrito) |>
          unique() |>
          sort()

        shinyWidgets::updatePickerInput(session, "filtro_distrito", selected = "", choices = district_list)
      } else {
        state$config$selected_dres = NULL
        # Distritos
        district_list <- setores$nm_distrito |>
          unique() |>
          sort()
        shinyWidgets::updatePickerInput(session, "filtro_distrito", choices = district_list, selected = "")

      }
    })

    observeEvent(input$filtro_distrito, ignoreNULL = FALSE, {
      if (!is.null(input$filtro_distrito)) {
        state$config$selected_districts = input$filtro_distrito

        # shinyWidgets::updatePickerInput(session, "filtro_dre", selected = "")
        # state$selected_dres = NULL
      } else {
        state$config$selected_districts = NULL
      }
    })

    observeEvent(input$mapa_variavel, {
      if (input$mapa_variavel %in% c("demanda", "deficit", "superavit")) {
        # demanda = população de estudantes
        # deficit e superavit = resultado da estimativa
        # remover opção de rede de ensino
        shinyWidgets::updatePickerInput(session, "mapa_rede", selected = "")
        if (length(input$mapa_ano) == 0) {
          shinyWidgets::updatePickerInput(session, "mapa_ano", selected = 2035)
        }
      }

      if (input$mapa_variavel == "oferta") {
        # oferta = número de vagas (matriculas)
        # remover opção de ano
        shinyWidgets::updatePickerInput(session, "mapa_ano", selected = "")
        if (length(input$mapa_rede) == 0) {
          shinyWidgets::updatePickerInput(session, "mapa_rede", selected = "publica")
        }
      }

      state$config$mapa_variavel = input$mapa_variavel

      # if (input$mapa_variavel == "resultado") {
      #   # resultado = déficit de vagas, calculado via bfca
      #   # remover opção de selecionar multiplas etapas
      #   if (length(input$mapa_etapa) > 1) {
      #     shinyWidgets::updatePickerInput(session, "mapa_etapa",
      #                                     selected = input$mapa_etapa[1])
      #   }
      # }

    })

    observeEvent(input$mapa_etapa, {
      if ((input$mapa_variavel == "resultado") & length(input$mapa_etapa) > 1) {
        showModal(modalDialog("Apenas uma etapa de ensino pode ser selecionada por vez para a visualização de déficit/superávit de vagas."))

        shinyWidgets::updatePickerInput(session, "mapa_etapa",
                                        selected = last(input$mapa_etapa))
      }

      state$config$mapa_etapa = input$mapa_etapa
    })

    observeEvent(input$mapa_ano, {
      segundo_ano = input$mapa_ano
      if (is.null(segundo_ano)) { segundo_ano = 2035 }

      state$config$anos = c(2020, segundo_ano)
    })

    # Shape data --------------------------------------------------------------
    map_shape <- reactive({
      req(input$unidade_espacial)

      state$config$unidade_espacial = input$unidade_espacial

      sf_shape <- NULL
      if (input$unidade_espacial == "hexgrid") {
        sf_shape <- dplyr::left_join(hexgrid, hexgrid_setor_lookup, by = "id_hex") |>
          dplyr::left_join(setores |> sf::st_set_geometry(NULL), by = "cd_setor")
      } else {
        sf_shape <- setores
      }

      sf_shape <- sf_shape |>
        dplyr::mutate(popup = glue::glue("<div><b>DRE: </b>{nm_dre}</div> <div><b>Distrito: </b>{nm_distrito}</div> <div><b>Setor: </b>{cd_setor}</div>"))

      if (!is.null(input$filtro_dre)) {
        sf_shape <- sf_shape |>
          dplyr::filter(nm_dre %in% input$filtro_dre)
      }

      if (!is.null(input$filtro_distrito)) {
        sf_shape <- sf_shape |>
          dplyr::filter(nm_distrito %in% input$filtro_distrito)
      }

      if (nrow(sf_shape) == 0) {
        return(NULL)
      } else {
        return(sf_shape)
      }

    })

    mapa_rede <- reactive({
      rede_ensino = input$mapa_rede
      if (is.null(rede_ensino)) { rede_ensino = "" }
      return(rede_ensino)
    })

    map_data <- reactive({
      req(input$mapa_variavel, input$unidade_espacial, input$mapa_etapa)
      mapa_rede()

      # browser()

      # state$config$mapa_variavel = input$mapa_variavel

      if (input$mapa_variavel == "demanda") {
        isolate({
          demanda <- populacao_por_hex |>
            dplyr::filter(serie %in% input$mapa_etapa) |>
            # dplyr::filter(serie %in% input$mapa_etapa, ano %in% state$config$anos) |>
            dplyr::group_by(id_hex, ano) |>
            dplyr::summarise(valor = sum(populacao_serie_hex), .groups = "drop")

          if (input$unidade_espacial == "setores_sme") {
            demanda <- dplyr::left_join(demanda, hexgrid_setor_lookup, by = "id_hex") |>
              dplyr::group_by(cd_setor, ano) |>
              dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
          }

          return(demanda)
        })
      }

      if (input$mapa_variavel == "oferta") {
        isolate({
          rede_ensino = input$mapa_rede
          if (is.null(rede_ensino)) { rede_ensino = "publica" }

          oferta <- matriculas_por_hex |>
            dplyr::filter(serie %in% input$mapa_etapa, rede == rede_ensino) |>
            dplyr::group_by(id_hex) |>
            dplyr::summarise(valor = sum(matriculas), .groups = "drop")

          if (input$unidade_espacial == "setores_sme") {
            oferta <- dplyr::left_join(oferta, hexgrid_setor_lookup, by = "id_hex") |>
              dplyr::group_by(cd_setor) |>
              dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
          }

          return(oferta)
        })
      }

      if (input$mapa_variavel == "deficit") {
        isolate({
          resultado <- deficit_bfca_hex |>
            # dplyr::filter(serie %in% input$mapa_etapa, ano %in% state$config$anos) |>
            dplyr::filter(serie %in% input$mapa_etapa) |>
            dplyr::group_by(id_hex, ano) |>
            dplyr::summarise(valor = sum(abs(deficit), na.rm = TRUE), .groups = "drop")

          if (input$unidade_espacial == "setores_sme") {
            resultado <- dplyr::left_join(resultado, hexgrid_setor_lookup, by = "id_hex") |>
              dplyr::group_by(cd_setor, ano) |>
              dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
          }

          return(resultado)
        })
      }

      if (input$mapa_variavel == "superavit") {
        isolate({
          resultado <- deficit_bfca_hex |>
            dplyr::filter(serie %in% input$mapa_etapa) |>
            # dplyr::filter(serie %in% input$mapa_etapa, ano %in% state$config$anos) |>
            dplyr::group_by(id_hex, ano) |>
            dplyr::summarise(valor = sum(superavit, na.rm = TRUE), .groups = "drop")

          if (input$unidade_espacial == "setores_sme") {
            resultado <- dplyr::left_join(resultado, hexgrid_setor_lookup, by = "id_hex") |>
              dplyr::group_by(cd_setor, ano) |>
              dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
          }

          return(resultado)
        })
      }
    })

    observeEvent(
      eventExpr = {
        map_data()
      },
      handlerExpr = {
        state$config$map_data = map_data()
      })

    observeEvent(
      eventExpr = {
        map_shape()
      },
      handlerExpr = {
        state$config$map_shape = map_shape()
      })

        ## Filter data -------------------------------
    school_data <- reactive({
      # req(state$state$id)

      # req(input$mapa_exibir_escolas)

      # Todas as escolas
      d <- escolas |>
        dplyr::filter(tp_categoria %in% input$mapa_exibir_escolas)

      # Filtrar DRE ou Distrito
      if (!is.null(state$config$selected_dres)) {
        if (length(state$config$selected_dres) > 0) {
          d <- d |>
            dplyr::filter(nm_dre %in% state$config$selected_dres)
        }
      }
      if (!is.null(state$config$selected_districts)) {
        if (length(state$config$selected_districts) > 0) {
          d <- d |>
            dplyr::filter(nm_distrito %in% state$config$selected_districts)
        }
      }

      d <- d |>
        dplyr::mutate(color = dplyr::case_when(tp_categoria == "Conveniada" ~ "#415A77FF",
                                               tp_categoria == "Municipal" ~ "#FFB703FF",
                                               tp_categoria == "Estadual" ~ "#C1121FFF"
        )) |>
        dplyr::mutate(popup = glue::glue("<div><b>Código (MEC): </b>{co_entidade}</div> <div><b>Escola: </b>{no_entidade}</div> <div><b>Rede: </b>{tp_categoria}</div> <div><b>Vagas: </b> <div><b>Creche: </b>{qt_mat_inf_cre}</div> <div><b>Pré-escola: </b>{qt_mat_inf_pre}</div> <div><b>Fundamental I: </b>{qt_mat_fund_ai}</div> <div><b>Fundamental II: </b>{qt_mat_fund_af}</div> "))

      # Limpar as colunas
      d <- d |>
        dplyr::select(cd_setor, nm_distrito, co_entidade, no_entidade, tp_categoria,
                      lat, lon, color, popup,
                      qt_mat_inf_cre, qt_mat_inf_pre, qt_mat_fund_ai, qt_mat_fund_af)


      return(d)

    })

    metro_data <- reactive({
      # req(input$mapa_exibir_metro)

      d <- metro

      d <- metro |>
        dplyr::filter(modo %in% input$mapa_exibir_metro) |>
        dplyr::mutate(popup = glue::glue("<div><b>Modo: </b>{modo}</div> <div><b>Corredor: </b>{corredor}</div> <div><b>Estação: </b>{estacao}</div>"))


      return(d)
    })

    observeEvent(
      eventExpr = {
        school_data()
      },
      handlerExpr = {
        state$config$schools = school_data()
      }
    )

    observeEvent(
      eventExpr = {
        metro_data()
      },
      handlerExpr = {
        state$config$metro = metro_data()
      }
    )


  })
}

## To be copied in the UI
# mod_diag_config_ui("diag_config_1")

## To be copied in the server
# mod_diag_config_server("diag_config_1")
