#' sim_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sim_table_ui <- function(id){
  ns <- NS(id)

  tabsetPanel(
    id = ns("tab_sim_map"),
    tabPanel(title = "Escolas",
             reactable::reactableOutput(ns("table_schools"), width = "100%", height = "100%")
    ),
    tabPanel(title = "Escolas Modificadas",
             reactable::reactableOutput(ns("table_modified_schools"), width = "100%", height = "100%")
    ),
    tabPanel(title = "Modificações em Lote",
             style = "margin: 5px; padding: 5px;",
             div(
               style = "margins: 5px; padding: 5px;",
               h4("Opção 1 - Carregar cenário anterior", class = "tile-headline"),
               p("Escolha um dos cenários criados anteriormente para importar modificações feitas anteriormente na capacidade das escolas. Esta opção permite continuar o trabalho começado em um cenårio anterior, ou então criar variações entre cenários previamente testados."),
               actionButton(ns("btn_load_scenario"), "Escolher cenário"),

               hr(),
               h4("Opção 2 - Modificações em lote", class = "tile-headline"),
               p("Esta opção permite modificar a capacidade de várias escolas de uma só vez. Deve ser executada em três passos:"),
               p("2.1 - Faça o download da planilha com as escolas existentes;"),
               downloadButton(ns("btn_download_schools_xls"), "Baixar planilha de escolas"),

               p(""),
               p("2.2 - Utilize o Microsoft Excel para preencher a nova capacidade das escolas;"),
               p("2.3 - Faça o upload da nova planilha para o sistema;"),
               fileInput(ns("btn_upload_schools_xls"),
                         label = "Carregar planilha com modificações",
                         buttonLabel = "Carregar planilha",
                         placeholder = "Nenhuma planilha selecionada",
                         accept = ".xlsx", width = "400px"),
             )
    )
  )

}

#' sim_table Server Functions
#'
#' @noRd
mod_sim_table_server <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Area selection ----------------------------------------------------------

    # filter_choices <- reactive({
    #   if (length(state$selected_dre) > 0) {
    #
    #   }
    #
    #   d <- setores |>
    #     sf::st_set_geometry(NULL) |>
    #     dplyr::select()
    #
    #   if (state$dre != app_states$INITIAL_DRE) {
    #     d <- d |> filter(nm_dre == state$dre)
    #   }
    #
    #   setNames(d$code, d$name)
    # })


    # Tabela de escolas -------------------------------------------------------

    ## Filter data -------------------------------
    df_escolas <- reactive({
      # req(state$state$id)

      # Todas as escolas
      d <- escolas

      # Filtrar DRE ou Distrito
      if (length(state$selected_dres) > 0) {
        d <- d |>
          dplyr::filter(nm_dre %in% state$selected_dres)
      }
      if (length(state$selected_districts) > 0) {
        d <- d |>
          dplyr::filter(nm_distrito %in% state$selected_districts)
      }

      # Limpar as colunas
      d <- d |>
        dplyr::select(cd_setor, nm_distrito, co_entidade, no_entidade, tp_categoria,
                      qt_mat_inf_cre, qt_mat_inf_pre, qt_mat_fund_ai, qt_mat_fund_af)


      return(d)

    })

    school_mod <- reactive({
      state$school_mod
    })


    ## Render table -------------------------------
    selected_row <- reactive({
      req(df_escolas())

      reactable::getReactableState("table_schools", "selected")
    })

    observeEvent(selected_row(), {
      row_index <- selected_row()

      if (is.null(row_index)) {
        state$school_selected <- -1
      } else {
        selected_school <- df_escolas()[row_index,]
        state$school_selected <- selected_school$co_entidade[1]
      }
    })


    footer_total <- function(values) format(sum(values, na.rm = TRUE),
                                            big.mark = ".", decimal.mark = ",")

    output$table_schools <- reactable::renderReactable({
      req(df_escolas(), state$window_height)

      reactable::reactable(
        df_escolas(),
        compact = TRUE,
        defaultColDef = reactable::colDef(#minWidth = 30,
                                          footerStyle = "font-weight: bold", resizable = TRUE),
        highlight = TRUE,
        selection = "single",
        defaultPageSize = round((state$window_height - 240) / 31),  # 345
        paginationType = "simple",
        searchable = TRUE,
        wrap = FALSE,
        onClick = "select", # onclick_js,
        defaultSorted = list(cd_setor = "asc", no_entidade = "asc"),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        ),

        columns = list(
          .selection = reactable::colDef(show = FALSE),
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE, minWidth = 75),
          nm_distrito = reactable::colDef(name = "Distrito", show = FALSE),
          co_entidade = reactable::colDef(name = "Código (MEC)", filterable = TRUE, show = TRUE),
          no_entidade = reactable::colDef(name = "Nome", filterable = TRUE, class = "area-link", minWidth = 200),
          tp_categoria = reactable::colDef(name = "Rede", filterable = TRUE, minWidth = 75),
          qt_mat_inf_cre = reactable::colDef(name = "Creche", footer = footer_total),# header = icon("car")),
          qt_mat_inf_pre = reactable::colDef(name = "Pré-escola", footer = footer_total),# header = icon("train")),
          qt_mat_fund_ai = reactable::colDef(name = "Fundamental I", footer = footer_total),# header = icon("walking")),
          qt_mat_fund_af = reactable::colDef(name = "Fundamental II", footer = footer_total)#, header = icon("bicycle"))
        ),
        columnGroups = list(
          reactable::colGroup(name = "Vagas", columns = c("qt_mat_inf_cre", "qt_mat_inf_pre", "qt_mat_fund_ai", "qt_mat_fund_af"))
        ),
        language = reactable::reactableLang(
          searchPlaceholder = "Pesquisar escolas",
          noData = "Nenhuma escola encontrada",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} escolas",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}"
        )
      )
    })


# Tabela de modificações --------------------------------------------------

    output$table_modified_schools <- reactable::renderReactable({
      req(school_mod(), state$window_height)

      sticky_style <- list(backgroundColor = "#f7f7f7")

      reactable::reactable(
        school_mod(),
        compact = TRUE,
        defaultColDef = reactable::colDef(#minWidth = 40,
                                          footerStyle = "font-weight: bold", resizable = TRUE),
        highlight = TRUE,
        defaultPageSize = round((state$window_height - 220) / 31),  # 345
        paginationType = "simple",
        searchable = TRUE,
        wrap = FALSE,
        # onClick = onclick_js,
        defaultSorted = list(no_entidade = "asc"),
        # theme = reactable::reactableTheme(
        #   rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        # ),

        columns = list(
          co_entidade = reactable::colDef(name = "Código (MEC)", filterable = FALSE, show = TRUE, sticky = "left", style = sticky_style,),
          no_entidade = reactable::colDef(name = "Nome", filterable = FALSE, class = "area-link", minWidth = 150, sticky = "left", style = sticky_style,),

          orig_mat_creche = reactable::colDef(name = "Qtde. Original", footer = footer_total),
          nova_mat_creche = reactable::colDef(name = "Nova Qtde.", footer = footer_total),

          orig_mat_pre = reactable::colDef(name = "Qtde. Original", footer = footer_total),
          nova_mat_pre = reactable::colDef(name = "Nova Qtde.", footer = footer_total),

          orig_mat_fund_ai = reactable::colDef(name = "Qtde. Original", footer = footer_total),
          nova_mat_fund_ai = reactable::colDef(name = "Nova Qtde.", footer = footer_total),

          orig_mat_fund_af = reactable::colDef(name = "Qtde. Original", footer = footer_total),
          nova_mat_fund_af = reactable::colDef(name = "Nova Qtde.", footer = footer_total)
        ),
        columnGroups = list(
          reactable::colGroup(name = "Vagas de Creche", columns = c("orig_mat_creche", "nova_mat_creche")),
          reactable::colGroup(name = "Vagas de Pré-escola", columns = c("orig_mat_pre", "nova_mat_pre")),
          reactable::colGroup(name = "Vagas de Fundamental I", columns = c("orig_mat_fund_ai", "nova_mat_fund_ai")),
          reactable::colGroup(name = "Vagas de Fundamental II", columns = c("orig_mat_fund_af", "nova_mat_fund_af"))
        ),
        language = reactable::reactableLang(
          searchPlaceholder = "Pesquisar escolas",
          noData = "Nenhuma escola encontrada",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} escolas",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}"
        )
      )

    })


# Load previous scenario --------------------------------------------------

    observeEvent(input$btn_load_scenario, {

      showModal(
        modalDialog(
          title = "Escolha um cenário",
          size = "l",
          mod_res_selector_ui("res_selector_load"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("btn_load"), "Carregar")
          )
        )
      )

    })

    observeEvent(input$btn_load, {
      # carregar modificações do cenário selecionado

      id_cenario <- state$scenario_selected

      modificacoes <- DBI::dbGetQuery(state$db_con,
                                      sprintf("SELECT * FROM modificacoes WHERE id_cenario = %d", id_cenario)) |>
        dplyr::select(co_entidade, no_entidade,
                      orig_mat_creche, nova_mat_creche,
                      orig_mat_pre, nova_mat_pre,
                      orig_mat_fund_ai, nova_mat_fund_ai,
                      orig_mat_fund_af, nova_mat_fund_af)

      state$school_mod <- state$school_mod |>
        dplyr::filter(!(co_entidade %in% modificacoes$co_entidade))

      state$school_mod <- rbind(state$school_mod, modificacoes)

      removeModal()

      showModal(
        modalDialog(
          title = "Operação concluída",
          easyClose = TRUE,
          "Modificações carregadas com sucesso. Verifique o resultado na aba 'Escolas Modificadas'",
          footer = modalButton("Fechar")
        )
      )



    })


# Download planilha ------------------------------------------------------


    output$btn_download_schools_xls <- downloadHandler(
      filename = "escolas.xlsx",

      content = function(file) {

        # faz cópia o template para o arquivo temporário
        file.copy("data/template_escolas.xlsx", file)

        # prepara dados das escolas
        schools_filtered <- dplyr::filter(escolas, co_entidade %in% df_escolas()$co_entidade) |>
          dplyr::mutate(qt_nova_mat_inf_cre = qt_mat_inf_cre,
                        qt_nova_mat_inf_pre = qt_mat_inf_pre,
                        qt_nova_mat_fund_ai = qt_mat_fund_ai,
                        qt_nova_mat_fund_af = qt_mat_fund_af) |>
          dplyr::select(co_entidade, no_entidade, tp_categoria, ds_endereco,
                        nm_dre, nr_distrito, nm_distrito, cd_setor,
                        qt_mat_inf_cre, qt_nova_mat_inf_cre,
                        qt_mat_inf_pre, qt_nova_mat_inf_pre,
                        qt_mat_fund_ai, qt_nova_mat_fund_ai,
                        qt_mat_fund_af, qt_nova_mat_fund_af,
                        qt_area_edificada, qt_area_livre_terreno,
                        qt_area_ocupada_terreno, qt_area_total_terreno,
                        qt_pavimentos, qt_salas_utilizadas, tmi_metro)

        # salva dados das escolas no template
        xl <- openxlsx::loadWorkbook(file)
        # openxlsx::addWorksheet(xl, "escolas")
        openxlsx::writeData(xl, sheet = "escolas", x = schools_filtered,
                            startRow = 7, colNames = FALSE)
        openxlsx::saveWorkbook(xl, file, overwrite = TRUE)
      })



# Upload das modificações -------------------------------------------------

  observeEvent(input$btn_upload_schools_xls, {

    tryCatch(
      expr = {
        # read uploaded file
        file_name <- input$btn_upload_schools_xls$datapath

        xl <- openxlsx::loadWorkbook(file_name)
        data <- openxlsx::readWorkbook(xl, sheet = "escolas", startRow = 7, colNames = FALSE, skipEmptyRows = TRUE)

        colnames(data) <- c("co_entidade", "no_entidade", "tp_categoria", "ds_endereco",
                            "nm_dre", "nr_distrito", "nm_distrito", "cd_setor",
                            "orig_mat_creche", "nova_mat_creche",
                            "orig_mat_pre", "nova_mat_pre",
                            "orig_mat_fund_ai", "nova_mat_fund_ai",
                            "orig_mat_fund_af", "nova_mat_fund_af",
                            "qt_area_edificada", "qt_area_livre_terreno",
                            "qt_area_ocupada_terreno", "qt_area_total_terreno",
                            "qt_pavimentos", "qt_salas_utilizadas", "tmi_metro")

        data <- data |> dplyr::filter(orig_mat_creche != nova_mat_creche |
                                        orig_mat_pre != nova_mat_pre |
                                        orig_mat_fund_ai != nova_mat_fund_ai |
                                        orig_mat_fund_af != nova_mat_fund_af ) |>
          dplyr::select(co_entidade, no_entidade,
                        orig_mat_creche, nova_mat_creche, orig_mat_pre, nova_mat_pre,
                        orig_mat_fund_ai, nova_mat_fund_ai, orig_mat_fund_af, nova_mat_fund_af)

        state$school_mod <- state$school_mod |>
          dplyr::filter(!(co_entidade %in% data$co_entidade))

        state$school_mod <- rbind(state$school_mod, data)

        showModal(
          modalDialog(
            title = "Operação concluída",
            easyClose = TRUE,
            "Modificações carregadas com sucesso. Verifique o resultado na aba 'Escolas Modificadas'",
            footer = modalButton("Fechar")
          )
        )
      },
      error = function(e) {
        showModal(
          modalDialog(
            title = "Falha na importação",
            easyClose = TRUE,
            "Erro ao carregar modificações. Verifique a se a planilha fornecida está no formato correto e tente novamente.",
            footer = modalButton("Fechar")
          )
        )
      }
    )

  })

  })
}

## To be copied in the UI
# mod_sim_table_ui("sim_table_1")

## To be copied in the server
# mod_sim_table_server("sim_table_1")
