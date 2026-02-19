aba3_ui <- function(id, base) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        uiOutput(ns("n_registros"))
      ),
      column(
        width = 2,
        div(
          class = "input-painel",
          pickerInput(
            inputId = ns("titulo2"),
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
            ns("area2"),
            "Área",
            c(
              "Estado" = "estado",
              "Município" = "municípios",
              "Região Administrativa" = "regiao_administrativa",
              "RISP" = "risp",
              "AISP" = "aisp",
              "CISP" = "cisp"
            )
          )
        )
      ),
      column(
        width = 2,
        div(
          class = "input-painel",
          pickerInput(
            inputId = ns("tipo2"),
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
        width = 2,
        div(
          class = "input-painel",
          airDatepickerInput(
            inputId = ns("periodo2"),
            label = "Período:",
            range = TRUE,
            addon = "none",
            view = "days",
            minView = "days",
            dateFormat = "dd/MM/yyyy",
            minDate = as.Date("2024-01-01"),
            maxDate = as.Date("2024-12-31"),
            value = c(
              as.Date("2024-01-01"),
              as.Date("2024-12-31")
            ),
            autoClose = TRUE,
            clearButton = TRUE,
            language = "pt"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        card(
          height = "680px",
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Ranking de Delitos por Título Selecionado"
          ),
          fluidRow(
            column(
              width = 6,
              downloadButton(ns("baixar_base2"), "Baixar Tabela CSV")
            )
          ),
          reactableOutput(ns("rank_del"))
        )
      ),
      column(
        width = 6,
        card(
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Distribuição dos Delitos por Dia da Semana e Hora do fato"
          ),
          echarts4rOutput(ns("heat_map"), height = "600px")
        )
      )
    )
  )
}

aba3_server <- function(id, base) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    base_filtrada2 <- reactive({
      
      req(
        input$titulo2,
        input$area2,
        input$tipo2,
        input$periodo2
      )
      
      base %>%
        filter(
          descricao_do %in% input$titulo2,
          .data[[input$area2]] %in% input$tipo2,
          data_com >= input$periodo2[1] & data_com <= input$periodo2[2]
        ) 
    })
    
    output$n_registros <- renderUI({

      req(
        input$titulo2,
        input$tipo2,
        input$periodo2[1],
        input$periodo2[2]
      )
      
      value_box_func(
        "Casos Registrados",
        base_filtrada2() %>%
          count(),
        "clipboard-list",
        "#08618f"
      )

    }) %>% 
      bindEvent(input$titulo2, input$tipo2, input$periodo2)
    
    output$rank_del <- renderReactable({
      
      req(
        input$titulo2,
        input$tipo2,
        input$periodo2[1],
        input$periodo2[2] 
      )

      name_col <- case_when(
        input$area2 == "estado" ~ "Estado",
        input$area2 == "municípios" ~ "Município",
        input$area2 == "regiao_administrativa" ~ "Região Administrativa",
        input$area2 == "risp" ~ "RISP",
        input$area2 == "aisp" ~ "AISP",
        input$area2 == "cisp" ~ "CISP"
      )

      base_filtrada2() %>%
        count(.data[[input$area2]], descricao_do) %>%
        mutate(
          perc = n/sum(n)*100,
        ) %>%
        arrange(
          descricao_do == "Outros",
          desc(n)
        ) %>% 
        mutate(
          x = 1:n(),
          cum_perc = cumsum(perc)
        ) %>%
        select(x, everything()) %>%
        rename(!!name_col := .data[[input$area2]]) %>%
        reactable(
          height = 550,
          defaultPageSize = 8,
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
            x = colDef(
              name = " ",
              width = 55
            ),
            descricao_do = colDef(name = "Delito"),
            n = colDef(
              name = "Observado",
              width = 100,
              style = list(fontWeight = "bold"),
              format = colFormat(
                separators = TRUE,
                digits = 0,
                locales = "pt-BR"
              )
            ),
            perc = colDef(
              name = "Percentual",
              width = 100,
              format = colFormat(
                separators = TRUE,
                digits = 1,
                locales = "pt-BR",
                suffix = "%"
              )
            ),
            cum_perc = colDef(
              name = "Percentual Acumulado",
              width = 110,
              format = colFormat(
                separators = TRUE,
                digits = 1,
                locales = "pt-BR",
                suffix = "%"
              )
            )
          )
        )
    }) %>% 
      bindEvent(input$titulo2, input$tipo2, input$periodo2)
    
    output$heat_map <- renderEcharts4r({

      req(
        input$titulo2,
        input$tipo2,
        input$periodo2[1],
        input$periodo2[2] 
      )
      
      base_filtrada2() %>%
        mutate(
          data_com = as.Date(data_com),
          dia_semana = wday(
            data_com,
            label = TRUE,
            abbr = FALSE,
            week_start = 7
          ),
          dia_semana = replace_na(dia_semana, "domingo"),
          hora_com = trimws(hora_com),
          hora_com = ifelse(hora_com == "", "00:00", hora_com),
          hora = hour(hms(hora_com))
        ) %>%
        count(hora, dia_semana) %>%
        mutate(
          dia_semana = case_when(
            dia_semana == "domingo" ~ "Domingo",
            dia_semana == "segunda-feira" ~ "Segunda",
            dia_semana == "terça-feira" ~ "Terça",
            dia_semana == "quarta-feira" ~ "Quarta",
            dia_semana == "quinta-feira" ~ "Quinta",
            dia_semana == "sexta-feira" ~ "Sexta",
            dia_semana == "sábado" ~ "Sábado"
          ),
          hora = as.character(hora),
          n = round(n/sum(n)*100, 2)
        ) %>% 
        e_charts(dia_semana) %>% 
        e_heatmap(
          hora,
          n,
          label = list(
            show = TRUE,
            formatter = htmlwidgets::JS("
              function(params){
                var val = params.value[2];

                var formatado = Number(val).toLocaleString('pt-BR', {
                  minimumFractionDigits: 2,
                  maximumFractionDigits: 2
                });

                return formatado + '%';
              }
            ")
          )
        ) %>% 
        e_x_axis(
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold"
          ),
          axisTick = list(show = FALSE)
        ) %>% 
        e_grid(top = 10, right = 10, bottom = 20, left = 90) %>% 
        e_visual_map(
          n,
          inRange = list(
            color = c("#fefdf8", "#f5eca8", "#bf444c")
          )
        ) %>% 
        e_y_axis(
          type = "category",
          inverse = TRUE,
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold",
            formatter = htmlwidgets::JS("
              function(value){
                return value + 'h';
              }
            ")
          ),
          axisTick = list(show = FALSE)
        )
    }) %>% 
      bindEvent(input$titulo2, input$tipo2, input$periodo2)
    
    observeEvent(input$area2, {
      
      labels_area2 <- case_when(
        input$area2 == "estado" ~ "Estado",
        input$area2 == "municípios" ~ "Município",
        input$area2 == "regiao_administrativa" ~ "Região Administrativa",
        input$area2 == "risp" ~ "RISP",
        input$area2 == "aisp" ~ "AISP",
        input$area2 == "cisp" ~ "CISP"
      )
      
      escolhas3 <- base %>% 
        select(.data[[input$area2]]) %>%
        distinct() %>% 
        pull() %>% 
        str_sort(numeric = TRUE)
      
      total_opcoes3 <- length(escolhas3)
      
      updatePickerInput(
        session = session,
        inputId = "tipo2",
        label = labels_area2,
        choices = escolhas3,
        selected = escolhas3,
        options = list(
          `selected-text-format` = paste0("count > ", total_opcoes3 - 1),
          `count-selected-text` = "Todos"
        )
      )
      
    })
    
    output$baixar_base2 <- downloadHandler(
      filename = function() {
        
        texto_download2 <- switch(
          input$area2,
          "estado" = "Estado",
          "municípios" = "Municípios",
          "regiao_administrativa" = "Região Administrativa",
          "risp" = "RISP",
          "aisp" = "AISP",
          "cisp" = "CISP"
        )
        
        paste0("ISP - ", input$titulo2, " - ", texto_download2, " - ", Sys.Date(), ".csv")
      },
      content = function(file) {
        
        dados2 <- base_filtrada2() %>%
          count(.data[[input$area2]], descricao_do) %>%
          mutate(
            perc = n/sum(n)*100,
          ) %>%
          arrange(
            descricao_do == "Outros",
            desc(n)
          ) %>% 
          mutate(
            x = 1:n(),
            cum_perc = cumsum(perc)
          ) %>%
          select(x, everything())
        
        write.csv2(dados2, file, row.names = FALSE, fileEncoding = "latin1")
      }
    )
    
  })
}