#' simulacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulacao_ui <- function(id) {
  ns <- NS(id)
  tagList(
    gridlayout::grid_container(
      layout = c("    1fr   400px   ",
                 "1fr data  edit    "),
      gridlayout::grid_card
      ("data",
        tabsetPanel(
          tabPanel(title = "Escolas", DT::dataTableOutput(ns("tabela_escolas"))),
          tabPanel(title = "Modificações", DT::dataTableOutput(ns(
            "tabela_escolas_modificadas"
          )))
        )),
      gridlayout::grid_nested
      (
        "edit",
        layout = c("280px agregado ",
                   "1fr   escola   "),
        gridlayout::grid_card("agregado",
                              tabsetPanel(
                                tabPanel(title = "Déficit por Distrito", DT::dataTableOutput(ns(
                                  "tabela_deficit_distrito"
                                ))),
                                tabPanel(title = "Déficit por Setor", DT::dataTableOutput(ns(
                                  "tabela_deficit_setor"
                                )))
                              )),
        gridlayout::grid_card(
          "escola",
          DT::dataTableOutput(ns("dados_escola")),

          actionButton(inputId = ns("btn_editar"),
                       label = "Editar Capacidade da Escola")
        )
      )
    )
  )
}


editar_escola_ui <- function(id) {
  ns <- NS(id)

}

#' simulacao Server Functions
#'
#' @noRd
mod_simulacao_server <- function(id, db_con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dados pré-carregados - deficit por setor e distrito ------------------------------------

    deficit_vagas <- reactiveValues(
      distritos = DBI::dbReadTable(db_con, "deficit_bfca_distrito"),
      setores = DBI::dbReadTable(db_con, "deficit_bfca_setor")
    )

    deficit_distrito_selecionado_df <- reactive({
      print(input$tabela_escolas_rows_selected)
      if (length(input$tabela_escolas_rows_selected)) {
        escola_selecionada <-
          table_df()[input$tabela_escolas_rows_selected,]

        dplyr::filter(deficit_vagas$distritos,
                      cd_distrito == escola_selecionada$cd_distrito[[1]])
      } else {
        return (NULL)
      }
    })

    # Dados pré-carregados - data.frame de modificações vazio ------------------------------

    modificacoes <- reactiveValues(
      escolas = data.frame(co_entidade = numeric(),
                           no_entidade = character(),
                           orig_mat_creche = numeric(),
                           nova_mat_creche = numeric(),

                           orig_mat_pre = numeric(),
                           nova_mat_pre = numeric(),

                           orig_mat_fund_ai = numeric(),
                           nova_mat_fund_ai = numeric(),

                           orig_mat_fund_af = numeric(),
                           nova_mat_fund_af = numeric()
      )
    )


    # Tabela de escolas -------------------------------------------------------
    escolas <- reactiveValues(data = DBI::dbReadTable(db_con, "escolas"))

    table_df <- reactive({
      escolas_df <-
        escolas$data |> dplyr::filter(tp_categoria != "privada")
      setores_df <-
        DBI::dbReadTable(db_con, "setores_sme") |> dplyr::select(-geometry)
      distritos_df <-
        DBI::dbReadTable(db_con, "distritos") |> dplyr::select(-geometry)

      escolas_joined_df <-
        dplyr::left_join(escolas_df,
                         setores_df,
                         by = c("co_setor" = "cd_setor", "cd_distrito")) |>
        dplyr::left_join(distritos_df,
                         by = c("cd_distrito", "cd_muni", "nr_distrito", "cd_dre"))

      escolas_clean_df <-
        dplyr::select(
          escolas_joined_df,
          cd_dre,
          nr_distrito,
          label_distrito,
          co_setor,
          co_entidade,
          no_entidade,
          tp_categoria,
          qt_mat_inf_cre,
          qt_mat_inf_pre,
          qt_mat_fund_ai,
          qt_mat_fund_af
        ) |>
        dplyr::mutate(cd_dre = stringr::str_remove(cd_dre, "DRE - ")) |>
        dplyr::arrange(co_setor, no_entidade)

      return(escolas_clean_df)
    })

    output$tabela_escolas <- DT::renderDataTable({
      # container com formatação extra para a tabela
      sketch = htmltools::withTags(table(class = 'display',
                                         thead(
                                           tr(
                                             th(colspan = 4, 'Localização'),
                                             th(colspan = 3, 'Escola'),
                                             th(colspan = 4, 'Matrículas')
                                           ),
                                           tr(
                                             th("DRE"),
                                             th(colspan = 2, "Distrito"),
                                             th("Setor"),
                                             th("Código (MEC)"),
                                             th("Nome"),
                                             th("Dependência"),
                                             th("Creche"),
                                             th("Pré-Escola"),
                                             th("Fundamental I"),
                                             th("Fundamental II")
                                           )
                                         )))

      DT::datatable(
        table_df(),
        container = sketch,
        rownames = FALSE,
        selection = "single",
        extensions = c("Responsive", "Scroller"),
        list(
          lengthMenu = c(5, 10, 15, 30, 50),
          pageLength = 15
        )
      )

    })

    # Escola selecionada na tabela ------------------------------

    vagas_escola_atual <- reactiveValues(
      cd_escola = -1,
      nm_escola = "",
      data =
        data.frame(
          serie = c("Creche", "Pré-Escola", "Fundamental I", "Fundamental II"),
          turmas = NA_integer_,
          vagas = NA_integer_,
          vagas_atualizadas = NA_integer_
        )
    )

    escola_atual <- reactive({
      if (length(input$tabela_escolas_rows_selected > 0)) {
        escola_selecionada <-
          table_df()[input$tabela_escolas_rows_selected[1],]

        escola_atual_df <- dplyr::filter(escolas$data,
                                         co_entidade == escola_selecionada$co_entidade[1])

        return(escola_atual_df)
      } else {
        return(dplyr::filter(escolas$data, co_entidade == -1))
      }
    })

    # Tabelas com déficit por setor e distrito da escola selecionada ----------
    output$tabela_deficit_distrito <- DT::renderDataTable({
      if (length(input$tabela_escolas_rows_selected > 0)) {
        escola_selecionada <-
          table_df()[input$tabela_escolas_rows_selected[1],]

        deficit_df <-
          dplyr::filter(
            deficit_vagas$distritos,
            nr_distrito == escola_selecionada$nr_distrito[1],
            cutoff == 15
          )

        caption_text = sprintf("Distrito %02d - %s",
                               deficit_df$nr_distrito[1],
                               deficit_df$nm_distrito[1])

        deficit_wide <-
          dplyr::select(deficit_df, ano, serie, deficit) |>
          tidyr::pivot_wider(names_from = ano, values_from = deficit)

        return(
          DT::datatable(
            deficit_wide,
            rownames = FALSE,
            selection = "none",
            filter = "none",
            caption = caption_text,
            options = list(dom = 't')
          )
        )

      } else {
        DT::datatable(data = NULL)
      }
    })

    output$tabela_deficit_setor <- DT::renderDataTable({
      if (length(input$tabela_escolas_rows_selected > 0)) {
        escola_selecionada <-
          table_df()[input$tabela_escolas_rows_selected[1],]

        deficit_df <-
          dplyr::filter(
            deficit_vagas$setores,
            cd_setor == escola_selecionada$co_setor[1],
            cutoff == 15
          )

        deficit_wide <-
          dplyr::select(deficit_df, ano, serie, deficit) |>
          tidyr::pivot_wider(names_from = ano, values_from = deficit)

        caption_text = sprintf("Setor %s - %s",
                               deficit_df$cd_setor[1],
                               deficit_df$nm_distrito[1])

        return(
          DT::datatable(
            deficit_wide,
            rownames = FALSE,
            selection = "none",
            filter = "none",
            caption = caption_text,
            options = list(dom = 't')
          )
        )

      } else {
        DT::datatable(data = NULL)
      }
    })


    # Características da escola selecionada -----------------------------------
    output$dados_escola <-  DT::renderDataTable({
      if (nrow(escola_atual()) == 1) {
        caption_text = sprintf("Escola %d - %s",
                               escola_atual()$co_entidade[1],
                               escola_atual()$no_entidade[1])

        dados_escola <- escola_atual() |>
          dplyr::mutate(endereco = paste0(ds_complemento, " ", ds_endereco, ", ", nu_endereco)) |>
          dplyr::select(
            #co_entidade, no_entidade,
            qt_area_edificada,
            qt_area_livre,
            qt_area_ocupada,
            qt_area_total,
            qt_pavimento,
            qt_salas,
            qt_salas_utilizadas_dentro
          ) |>
          # dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) |>
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "info",
            values_to = "valor"
          )


        dados_escola$info <- factor(
          dados_escola$info,
          levels = c(
            "qt_area_edificada",
            "qt_area_livre",
            "qt_area_ocupada",
            "qt_area_total",
            "qt_pavimento",
            "qt_salas",
            "qt_salas_utilizadas_dentro"
          ),
          labels = c(
            "Área edificada",
            "Área livre no terreno",
            "Área ocupada no terreno",
            "Área total do terreno",
            "Número de pavimentos",
            "Número de salas",
            "Número de salas utilizadas"
          )
        )

        return(
          DT::datatable(
            dados_escola,
            rownames = FALSE,
            colnames = "",
            selection = "none",
            filter = "none",
            caption = caption_text,
            options = list(dom = 't')
          )
        )
      } else {
        DT::datatable(data = NULL)
      }
    })


    # Modal window - edit school ----------------------------------------------


    update_vagas_para_editar <- function()  {
      vagas_escola_atual$cd_escola = escola_atual()$co_entidade[1]
      vagas_escola_atual$nm_escola = escola_atual()$no_entidade[1]

      vagas_escola_atual$data$turmas <-
        c(escola_atual()$qt_tur_inf_cre[1],
          escola_atual()$qt_tur_inf_pre[1],
          escola_atual()$qt_tur_fund_ai[1],
          escola_atual()$qt_tur_fund_af[1]
        )

      vagas_escola_atual$data$vagas <-
        c(escola_atual()$qt_mat_inf_cre[1],
          escola_atual()$qt_mat_inf_pre[1],
          escola_atual()$qt_mat_fund_ai[1],
          escola_atual()$qt_mat_fund_af[1]
        )

      vagas_escola_atual$data$vagas_atualizadas =
        c(escola_atual()$qt_mat_inf_cre[1],
          escola_atual()$qt_mat_inf_pre[1],
          escola_atual()$qt_mat_fund_ai[1],
          escola_atual()$qt_mat_fund_af[1]
        )
    }

    observeEvent(input$btn_editar, {
      if (nrow(escola_atual()) == 1) {

        update_vagas_para_editar()

        showModal(modalDialog(
          size = "l",
          gridlayout::grid_container(
            layout = c("      1fr  ",
                       "100px header",
                       "280px data",
                       "100px footer"),
            gridlayout::grid_card_text("header",
                                       wrapping_tag = "h5",
                                       paste0(
                                         vagas_escola_atual$cd_escola,
                                         " - ",
                                         vagas_escola_atual$nm_escola
                                       )
            ),
            gridlayout::grid_card("data", DT::dataTableOutput(ns(
              "vagas_para_editar"
            ))),
            gridlayout::grid_card_text("footer",
                                       wrapping_tag = "p",
                                       "Para editar o número de vagas disponíveis na escola, dê um clique duplo na célula com o valor desejado na coluna 'Vagas Modificadas'. Os valores de turmas e vagas atuais não podem ser modificados."
            )
          ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("btn_salvar_edicao"), "Salvar")
          )
        ))


      } else {
        showModal(
          modalDialog(
            title = "Selecione uma escola",
            "Você deve selecionar uma escola para editar a capacidade.",
            footer = modalButton("Fechar")
          )
        )

      }
    })

    output$vagas_para_editar <- DT::renderDataTable({
      return(
        DT::datatable(
          vagas_escola_atual$data,
          rownames = FALSE,
          selection = "none",
          filter = "none",
          caption = "",
          colnames = c("Turmas Atuais", "Vagas Atuais", "Vagas Modificadas"),
          editable = list(
            target = "cell",
            numeric = 3,
            disable = list(columns = c(1, 2))
          ),
          options = list(dom = 't')
        )
      )
    })

    observeEvent(input$vagas_para_editar_cell_edit, {
      row  <- input$vagas_para_editar_cell_edit$row
      clmn <- input$vagas_para_editar_cell_edit$col + 1
      vagas_escola_atual$data[row, clmn] <- as.integer(input$vagas_para_editar_cell_edit$value)
    })



    # observeEvent(input$vagas_para_editar_cell_edit, {
    #   vagas_escola_atual$data[input$vagas_para_editar_cell_edit$row,input$vagas_para_editar_cell_edit$col] <<- input$vagas_para_editar_cell_edit$value
    # })

    observeEvent(input$btn_salvar_edicao, {

      # dataframe editado pelo usuário
      codigo_escola <- vagas_escola_atual$cd_escola
      nome_escola <- vagas_escola_atual$nm_escola
      vagas_editadas <- vagas_escola_atual$data


      # dataframe no formato da tabela de modificações
      escolas_mod = data.frame(co_entidade = codigo_escola,
                               no_entidade = nome_escola,
                               orig_mat_creche = vagas_editadas$vagas[1],
                               nova_mat_creche = vagas_editadas$vagas_atualizadas[1],

                               orig_mat_pre = vagas_editadas$vagas[2],
                               nova_mat_pre = vagas_editadas$vagas_atualizadas[2],

                               orig_mat_fund_ai = vagas_editadas$vagas[3],
                               nova_mat_fund_ai = vagas_editadas$vagas_atualizadas[3],

                               orig_mat_fund_af = vagas_editadas$vagas[4],
                               nova_mat_fund_af = vagas_editadas$vagas_atualizadas[4]
      )

      modificacoes$escolas <- rbind(modificacoes$escolas, escolas_mod)

      removeModal()
    })

    output$tabela_escolas_modificadas <- DT::renderDataTable({
      # container com formatação extra para a tabela
      sketch = htmltools::withTags(table(class = 'display',
                                         thead(
                                           tr(
                                             th(colspan = 2, 'Escola'),
                                             th(colspan = 2, 'Creche'),
                                             th(colspan = 2, 'Pré-Escola'),
                                             th(colspan = 2, 'Fundamental I'),
                                             th(colspan = 2, 'Fundamental II')
                                           ),
                                           tr(
                                             th("Código (MEC)"),
                                             th("Nome"),
                                             th("Vagas Atuais"),
                                             th("Vagas Editadas"),
                                             th("Vagas Atuais"),
                                             th("Vagas Editadas"),
                                             th("Vagas Atuais"),
                                             th("Vagas Editadas"),
                                             th("Vagas Atuais"),
                                             th("Vagas Editadas")
                                           )
                                         )))

      DT::datatable(
        modificacoes$escolas,
        container = sketch,
        rownames = FALSE,
        selection = "single",
        extensions = c("Responsive", "Scroller"),
        list(
          lengthMenu = c(5, 10, 15, 30, 50),
          pageLength = 15
        )
      )

    })



  })
}

## To be copied in the UI
# mod_simulacao_ui("simulacao_1")

## To be copied in the server
# mod_simulacao_server("simulacao_1")
