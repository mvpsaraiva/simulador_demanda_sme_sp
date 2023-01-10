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
    tabPanel(title = "Escolas Adicionadas",
             div(
               h4("Adicionar escola", class = "tile-headline"),
               # p("Opção 1 - Informe as coordenadas geográficas da nova escola, separadas por vírgula (podem ser copiadas do Google Maps, p. ex.); ou"),
               p("1. Clique no mapa ao lado para preencher as coordenadas;"),
               p("2. Clique em ADICIONAR ESCOLA para preencher o restante dos dados."),
               splitLayout(
                 textInput(ns("text_coordenadas"), label = "Coordenadas", placeholder = "-23.550620, -46.633298"),
                 # numericInput(ns("num_lat"), label = "Lat", value = -23.64986),
                 # numericInput(ns("num_lon"), label = "Lon", value = -46.64806),
                 actionButton(inputId = ns("btn_add_school"), label = "Adicionar Escola")
               ),
             ),
             # hr(),
             div(
               p("Selecione abaixo as escolas a serem ativadas nesta simulação."),
               reactable::reactableOutput(ns("table_added_schools"), width = "100%", height = "60vh"),
               splitLayout(
                 cellWidths = 150,
                 actionButton(inputId = ns("btn_edit_school"), label = "Editar Escola"),
                 actionButton(inputId = ns("btn_delete_school"), label = "Excluir Escola")
               )
             )
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

    school_add <- reactive({
      state$school_add
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

    footer_total <- reactable::JS(
      "function(column, state) {
          let total = 0
          state.sortedData.forEach(function(row) {
            total += row[column.id]
          })
          return total.toLocaleString(2)
        }"
      )

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


# Escolas Adicionadas -----------------------------------------------------

    new_school_coords <- reactive({
      state$new_school_coords
    })

    selected_rows_added_school <- reactive({
      req(school_add())

      table_state <- reactable::getReactableState("table_added_schools")
      return(table_state$selected)
    })

    observeEvent(selected_rows_added_school(), {
      row_index <- selected_rows_added_school()

      if (is.null(row_index)) {
        state$added_school_selected <- -1
      } else {
        selected_school <- school_add()[row_index,]
        state$added_school_selected <- selected_school$co_entidade
      }
    })


    output$table_added_schools <- reactable::renderReactable({
      req(school_add(), state$window_height)

      schools_df <- school_add() |>
        dplyr::select(co_entidade, no_entidade, nm_dre, nm_distrito, cd_setor,
                      qt_mat_inf_cre, qt_mat_inf_pre, qt_mat_fund_ai, qt_mat_fund_af)

      sticky_style <- list(backgroundColor = "#f7f7f7")

      reactable::reactable(
        schools_df,
        compact = TRUE,
        defaultColDef = reactable::colDef(#minWidth = 40,
          footerStyle = "font-weight: bold", resizable = TRUE),
        highlight = TRUE,
        # defaultPageSize = round((state$window_height - 500) / 31),  # 345
        # paginationType = "simple",
        # showPagination = TRUE,
        pagination = FALSE,
        searchable = TRUE,
        wrap = FALSE,
        selection = "multiple",
        # onClick = onclick_js,
        defaultSorted = list(co_entidade = "asc"),

        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        ),

        columns = list(
          co_entidade = reactable::colDef(name = "Código", filterable = TRUE, show = TRUE), #, sticky = "left", style = sticky_style),
          no_entidade = reactable::colDef(name = "Nome", filterable = TRUE, class = "area-link", minWidth = 150),

          nm_dre = reactable::colDef(name = "DRE", filterable = TRUE, minWidth = 70),
          nm_distrito = reactable::colDef(name = "Distrito", filterable = TRUE, minWidth = 70),
          cd_setor = reactable::colDef(name = "Setor", filterable = TRUE, minWidth = 70),

          qt_mat_inf_cre = reactable::colDef(name = "Creche", footer = footer_total),
          qt_mat_inf_pre = reactable::colDef(name = "Pré-escola", footer = footer_total),
          qt_mat_fund_ai = reactable::colDef(name = "Fundamental I", footer = footer_total),
          qt_mat_fund_af = reactable::colDef(name = "Fundamental II", footer = footer_total)
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

    observeEvent(new_school_coords(), {
      isolate(updateTextInput(session, "text_coordenadas", value = new_school_coords()))
    })


# add school --------------------------------------------------------------


    observeEvent(input$btn_add_school, {

      state$add_school = list()

      # extrair coordenadas do input
      coords <- input$text_coordenadas
      coords_n <- strsplit(coords, split = ",") |> unlist() |> as.numeric()
      coord_lat <- coords_n[1]
      coord_lon <- coords_n[2]

      is_valid_lat <- is.finite(coord_lat) & abs(coord_lat) <= 90
      is_valid_lon <- is.finite(coord_lon) & abs(coord_lon) <= 180

      if ((is_valid_lat & is_valid_lon) == FALSE) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "As coordenadas informadas são inválidas."
        ))

        return(NULL)
      }

      # identificar se as coordenadas estão dentro da área de estudo
      hex <- h3jsr::point_to_cell(input = c(coord_lon, coord_lat), res = 9)
      is_valid <- hex %in% hexgrid$id_hex |> unlist()

      if (is_valid == FALSE) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "As coordenadas informadas encontram-se fora da área de estudo."
        ))

        return(NULL)
      }

      localizacao_df <- hexgrid |>
        sf::st_set_geometry(NULL) |>
        dplyr::filter(id_hex == hex) |>
        dplyr::left_join(hexgrid_setor_lookup) |>
        dplyr::left_join(tmi_metro) |>
        dplyr::left_join(sf::st_set_geometry(setores, NULL))

      deficit_df <- ttm |>
        dplyr::filter(from_id == hex, travel_time <= 15) |>
        dplyr::left_join(deficit_bfca_hex, by = c("to_id" = "id_hex")) |>
        dplyr::filter(ano == 2020) |>
        dplyr::group_by(serie) |>
        dplyr::summarise(deficit = sum(deficit)) |>
        tidyr::pivot_wider(names_from = serie, values_from = deficit)

      if (nrow(deficit_df) == 0) {
        deficit_df <- data.frame(
          qt_mat_inf_cre = 0,
          qt_mat_inf_pre = 0,
          qt_mat_fund_ai = 0,
          qt_mat_fund_af = 0
        )
      }

      state$add_school = list(
        co_entidade = 0,
        no_entidade = paste(localizacao_df$nm_dre[1], localizacao_df$nm_distrito[1], sep = " - "),
        cd_setor = localizacao_df$cd_setor[1],
        nr_distrito = localizacao_df$nr_distrito[1],
        nm_distrito = localizacao_df$nm_distrito[1],
        cd_dre = localizacao_df$cd_dre[1],
        nm_dre = localizacao_df$nm_dre[1],
        id_hex = hex,
        lat = coord_lat,
        lon = coord_lon,

        qt_mat_inf_cre = abs(deficit_df$creche[1]),
        qt_mat_inf_pre = abs(deficit_df$pre[1]),
        qt_mat_fund_ai = abs(deficit_df$anos_iniciais[1]),
        qt_mat_fund_af = abs(deficit_df$anos_finais[1]),

        tmi_metro = localizacao_df$travel_time[1]

        # new_school = TRUE
      )

      state$new_school = state$new_school + 1

      showModal(modalDialog(
        size = "m",
        easyClose = TRUE,
        mod_sim_add_school_ui("sim_add_school"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_add"), "Adicionar")
        )
      ))


    })

    observeEvent(input$btn_add, {

      nova_escola <- data.frame(
        co_entidade = 0,
        no_entidade = state$add_school$no_entidade,
        tp_categoria = "Municipal",
        # ds_endereco = "",
        cd_setor = state$add_school$cd_setor,
        nr_distrito = state$add_school$nr_distrito,
        nm_distrito = state$add_school$nm_distrito,
        cd_dre = state$add_school$cd_dre,
        nm_dre = state$add_school$nm_dre,
        id_hex = state$add_school$id_hex,
        lat = state$add_school$lat,
        lon = state$add_school$lon,

        qt_mat_inf_cre = state$add_school$qt_mat_inf_cre,
        qt_mat_inf_pre = state$add_school$qt_mat_inf_pre,
        qt_mat_fund_ai = state$add_school$qt_mat_fund_ai,
        qt_mat_fund_af = state$add_school$qt_mat_fund_af,

        tmi_metro = state$add_school$tmi_metro
      ) |>
        # converte nome da escola para maiúsculas, para combinar com informações do censo escolar
        dplyr::mutate(no_entidade = toupper(no_entidade))

      state$school_add <- adiciona_escola(state$db_con, nova_escola)
      state$add_school = list()

      removeModal()
    })


# delete school -----------------------------------------------------------


    observeEvent(input$btn_delete_school, {

      if (is.null(state$added_school_selected)) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "Selecione ao menos uma escola para excluí-la."
        ))

        return(NULL)
      }

      if (-1 %in% state$added_school_selected) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "Selecione ao menos uma escola para editá-la."
        ))

        return(NULL)
      }

      showModal(modalDialog(
        size = "s",
        easyClose = TRUE,
        sprintf("Confirma a exclusão de %s escola(s) selecionada(s)?", length(state$added_school_selected)),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_delete_school_confirm"), "Confirmar")
        )
      ))
    })

    observeEvent(input$btn_delete_school_confirm, {
      state$school_add <- remove_escolas(state$db_con, state$added_school_selected)
      state$added_school_selected <- c()

      removeModal()
    })


# edit school -------------------------------------------------------------


    observeEvent(input$btn_edit_school, {

      if (is.null(state$added_school_selected)) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "Selecione uma escola para editá-la."
        ))

        return(NULL)
      }

      if (-1 %in% state$added_school_selected) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "Selecione uma escola para editá-la."
        ))

        return(NULL)
      }

      if (length(state$added_school_selected) > 1) {
        showModal(modalDialog(
          size = "s",
          easyClose = TRUE,
          "Selecione apenas uma escola para editá-la."
        ))

        return(NULL)
      }

      school_to_edit <- state$school_add |>
        dplyr::filter(co_entidade %in% state$added_school_selected)

      state$add_school = list(
        co_entidade = school_to_edit$co_entidade[1],
        no_entidade = school_to_edit$no_entidade[1],
        cd_setor = school_to_edit$cd_setor[1],
        nr_distrito = school_to_edit$nr_distrito[1],
        nm_distrito = school_to_edit$nm_distrito[1],
        cd_dre = school_to_edit$cd_dre[1],
        nm_dre = school_to_edit$nm_dre[1],
        id_hex = school_to_edit$id_hex[1],
        lat = school_to_edit$lat[1],
        lon = school_to_edit$lon[1],

        qt_mat_inf_cre = school_to_edit$qt_mat_inf_cre[1],
        qt_mat_inf_pre = school_to_edit$qt_mat_inf_pre[1],
        qt_mat_fund_ai = school_to_edit$qt_mat_fund_ai[1],
        qt_mat_fund_af = school_to_edit$qt_mat_fund_af[1],

        tmi_metro = school_to_edit$tmi_metro[1]
      )

      state$new_school = state$new_school + 1

      showModal(modalDialog(
        size = "m",
        easyClose = TRUE,
        mod_sim_add_school_ui("sim_add_school"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_edit_school_confirm"), "Salvar")
        )
      ))
    })

    observeEvent(input$btn_edit_school_confirm, {

      nova_escola <- data.frame(
        co_entidade = state$add_school$co_entidade,
        no_entidade = state$add_school$no_entidade,
        tp_categoria = "Municipal",
        # ds_endereco = "",
        cd_setor = state$add_school$cd_setor,
        nr_distrito = state$add_school$nr_distrito,
        nm_distrito = state$add_school$nm_distrito,
        cd_dre = state$add_school$cd_dre,
        nm_dre = state$add_school$nm_dre,
        id_hex = state$add_school$id_hex,
        lat = state$add_school$lat,
        lon = state$add_school$lon,

        qt_mat_inf_cre = state$add_school$qt_mat_inf_cre,
        qt_mat_inf_pre = state$add_school$qt_mat_inf_pre,
        qt_mat_fund_ai = state$add_school$qt_mat_fund_ai,
        qt_mat_fund_af = state$add_school$qt_mat_fund_af,

        tmi_metro = state$add_school$tmi_metro
      ) |>
        # converte nome da escola para maiúsculas, para combinar com informações do censo escolar
        dplyr::mutate(no_entidade = toupper(no_entidade))

      state$school_add <- edita_escola(state$db_con, nova_escola)
      state$add_school = list()

      removeModal()
    })

# Load previous scenario --------------------------------------------------

    observeEvent(input$btn_load_scenario, {

      showModal(
        modalDialog(
          title = "Escolha um cenário",
          size = "l",
          mod_res_selector_ui("res_selector_load", w = "400px"),
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

      # carregar escolas adicionadas do cenário selecionado
      adicoes <- DBI::dbGetQuery(state$db_con,
                                      sprintf("SELECT * FROM adicoes WHERE id_cenario = %d", id_cenario))

      state$add_school = adicoes$co_entidade |> unlist()
      indices <- match(adicoes$co_entidade, state$school_add$co_entidade)

      reactable::updateReactable(ns("table_added_schools"), selected = indices)

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
