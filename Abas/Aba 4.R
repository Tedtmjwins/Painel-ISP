aba4_ui <- function(id, base) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 2,
        div(
          class = "input-painel",
          pickerInput(
            inputId = ns("titulo4"),
            label = "Título",
            choices = base %>% distinct(descricao_do) %>%
              arrange(
                descricao_do == "Outros",
                descricao_do
              ) %>% pull(),
            selected = "Ameaça",
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `select-all-text` = "Todas",
              `deselect-all-text` = "Nenhum",
              `none-selected-text` = "Selecione uma opção",
              `selected-text-format` = "count > 66",
              `count-selected-text` = "Todos",
              `live-search` = TRUE,
              size = 7
            )
          )
        )
      ),
      column(
        width = 2,
        div(
          class = "input-painel",
          selectInput(
            ns("area4"),
            "Área",
            c(
              "Estado" = "estado",
              "Município" = "municípios",
              "Região Administrativa" = "regiao_administrativa",
              "RISP" = "risp",
              "AISP" = "aisp",
              "CISP" = "cisp"
            ),
            width = "100%"
          )
        )
      ),
      column(
        width = 2,
        div(
          class = "input-painel",
          pickerInput(
            inputId = ns("tipo4"),
            label = "Estado",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `select-all-text` = "Todas",
              `deselect-all-text` = "Nenhum",
              `none-selected-text` = "Selecione uma opção",
              `live-search` = TRUE,
              size = 7
            )
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "input-painel",
          airDatepickerInput(
            inputId = ns("periodo41"),
            label = "Período 1:",
            range = TRUE,
            addon = "none",
            view = "months",
            minView = "months",
            dateFormat = "MM/yyyy",
            minDate = as.Date("2024-01-01"),
            maxDate = as.Date("2024-12-31"),
            value = c(
              as.Date("2024-01-01"),
              as.Date("2024-06-01")
            ),
            autoClose = TRUE,
            clearButton = TRUE,
            language = "pt",
            width = "100%"
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "input-painel",
          airDatepickerInput(
            inputId = ns("periodo42"),
            label = "Período 2:",
            range = TRUE,
            addon = "none",
            view = "months",
            minView = "months",
            dateFormat = "MM/yyyy",
            minDate = as.Date("2024-01-01"),
            maxDate = as.Date("2024-12-31"),
            value = c(
              as.Date("2024-07-01"),
              as.Date("2024-12-01")
            ),
            autoClose = TRUE,
            clearButton = TRUE,
            language = "pt",
            width = "100%"
          )
        )
      ),
    ),
    fluidRow(
      column(
        width = 12,
        card(
          height = "750px",
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Comparativo"
          ),
          fluidRow(
            column(
              width = 6,
              downloadButton(ns("baixar_base4"), "Baixar Tabela CSV")
            )
          ),
          reactableOutput(ns("comparativo"))
        )
      )
    )
  )
}

aba4_server <- function(id, base) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$comparativo <- renderReactable({
      
      req(
        input$titulo4,
        input$tipo4,
        input$periodo41[1],
        input$periodo41[2],
        input$periodo42[1],
        input$periodo42[2] 
      )
      
      validate(
        need(
          !any(
            month(input$periodo41[1]):month(input$periodo41[2]) %in%
              month(input$periodo42[1]):month(input$periodo42[2])
          ),
          "Os intervalos não podem se sobrepor"
        )
      )
      
      name_col2 <- case_when(
        input$area4 == "estado" ~ "Estado",
        input$area4 == "municípios" ~ "Município",
        input$area4 == "regiao_administrativa" ~ "Região Administrativa",
        input$area4 == "risp" ~ "RISP",
        input$area4 == "aisp" ~ "AISP",
        input$area4 == "cisp" ~ "CISP"
      )
      
      left_join(
        base %>% 
          filter(
            descricao_do %in% input$titulo4,
            .data[[input$area4]] %in% input$tipo4,
            mes %in% month(input$periodo41[1]):month(input$periodo41[2])
          ) %>%
          group_by(.data[[input$area4]], descricao_do) %>% 
          count() %>% 
          rename(periodo_1 = n),
        base %>% 
          filter(
            descricao_do %in% input$titulo4,
            .data[[input$area4]] %in% input$tipo4,
            mes %in% month(input$periodo42[1]):month(input$periodo42[2])
          ) %>%
          group_by(.data[[input$area4]], descricao_do) %>% 
          count() %>% 
          rename(
            periodo_2 = n
          ),
        by = c(input$area4, "descricao_do")
      ) %>% 
        mutate(
          periodo_1 = replace_na(periodo_1, 0),
          periodo_2 = replace_na(periodo_2, 0),
          diff_abs = periodo_2-periodo_1,
          diff_perc = ((periodo_2/periodo_1)-1)*100
        ) %>%
        arrange(
          descricao_do == "Outros",
          desc(periodo_2)
        ) %>% 
        rename(!!name_col2 := .data[[input$area4]]) %>% 
        reactable(
          height = 630,
          defaultPageSize = 10,
          defaultColDef = colDef(
            headerStyle = list(
              fontWeight = "bold"
            )
          ),
          style = list(
            backgroundColor = "#080b8100",
            color = "#080b81"
          ),
          columns = list(
            descricao_do = colDef(
              name = "Delito"
            ),
            periodo_1 = colDef(
              name = "Período 1",
              format = colFormat(
                separators = TRUE,
                digits = 0,
                locales = "pt-BR"
              ),
              style = list(
                color = "#080b81",
                fontWeight = "bold"
              )
            ),
            periodo_2 = colDef(
              name = "Período 2",
              format = colFormat(
                separators = TRUE,
                digits = 0,
                locales = "pt-BR"
              ),
              style = list(
                color = "#080b81",
                fontWeight = "bold"
              )
            ),
            diff_abs = colDef(
              name = "Diferença Absoluta",
              format = colFormat(
                separators = TRUE,
                digits = 0,
                locales = "pt-BR"
              ),
              style = function(value) {
                if (value >= 0) {
                  color <- "#0000b3"
                } else {
                  color <- "#e6b809"
                } 
                list(color = color, fontWeight = "bold")
              }
            ),
            diff_perc = colDef(
              name = "Diferença Percentual",
              format = colFormat(
                separators = TRUE,
                digits = 1,
                locales = "pt-BR",
                suffix = "%"
              ),
              style = function(value) {
                if (value >= 0) {
                  color <- "#0000b3"
                } else {
                  color <- "#e6b809"
                } 
                list(color = color, fontWeight = "bold")
              }
            )
          )
        )
      
    }) %>% 
      bindEvent(input$titulo4, input$tipo4, input$periodo41, input$periodo42)
    
    observeEvent(input$area4, {
      
      labels_area4 <- case_when(
        input$area4 == "estado" ~ "Estado",
        input$area4 == "municípios" ~ "Município",
        input$area4 == "regiao_administrativa" ~ "Região Administrativa",
        input$area4 == "risp" ~ "RISP",
        input$area4 == "aisp" ~ "AISP",
        input$area4 == "cisp" ~ "CISP"
      )
      
      escolhas4 <- base %>% 
        select(.data[[input$area4]]) %>%
        distinct() %>% 
        pull() %>% 
        str_sort(numeric = TRUE)
      
      total_opcoes4 <- length(escolhas4)
      
      updatePickerInput(
        session = session,
        inputId = "tipo4",
        label = labels_area4,
        choices = escolhas4,
        selected = escolhas4,
        options = list(
          `selected-text-format` = paste0("count > ", total_opcoes4 - 1),
          `count-selected-text` = "Todos"
        )
      )
      
    })
    
    output$baixar_base4 <- downloadHandler(
      filename = function() {
        
        texto_download4 <- switch(
          input$area4,
          "estado" = "Estado",
          "municípios" = "Municípios",
          "regiao_administrativa" = "Região Administrativa",
          "risp" = "RISP",
          "aisp" = "AISP",
          "cisp" = "CISP"
        )
        
        paste0("ISP - ", input$titulo4, " - ", texto_download4, " - ", Sys.Date(), ".csv")
      },
      content = function(file) {
        
        dados4 <- left_join(
          base %>% 
            filter(
              descricao_do %in% input$titulo4,
              .data[[input$area4]] %in% input$tipo4,
              mes %in% month(input$periodo41[1]):month(input$periodo41[2])
            ) %>%
            group_by(.data[[input$area4]], descricao_do) %>% 
            count() %>% 
            rename(periodo_1 = n),
          base %>% 
            filter(
              descricao_do %in% input$titulo4,
              .data[[input$area4]] %in% input$tipo4,
              mes %in% month(input$periodo42[1]):month(input$periodo42[2])
            ) %>%
            group_by(.data[[input$area4]], descricao_do) %>% 
            count() %>% 
            rename(periodo_2 = n),
          by = c(input$area4, "descricao_do")
        ) %>% 
          mutate(
            periodo_1 = replace_na(periodo_1, 0),
            periodo_2 = replace_na(periodo_2, 0),
            diff_abs = periodo_2-periodo_1,
            diff_perc = ((periodo_2/periodo_1)-1)*100
          ) %>%
          arrange(
            descricao_do == "Outros",
            desc(periodo_2)
          )
        
        write.csv2(dados4, file, row.names = FALSE, fileEncoding = "latin1")
      }
    )
    
  })
}