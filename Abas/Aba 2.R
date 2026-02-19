aba2_ui <- function(id, base) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        div(
          class = "input-painel",
          selectInput(
            ns("titulo"),
            "Título",
            base %>% distinct(descricao_do) %>%
              arrange(
                descricao_do == "Outros",
                descricao_do
              ) %>% pull(),
            width = "100%"
          ),
        )
      ),
      column(
        width = 3,
        div(
          class = "input-painel",
          selectInput(
            ns("area"),
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
          ),
        )
      ),
      column(
        width = 3,
        div(
          class = "input-painel",
          pickerInput(
            inputId = ns("tipo"),
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
            ),
            width = "100%"
          ),
        )
      ),
      column(
        width = 3,
        div(
          class = "input-painel",
          airDatepickerInput(
            inputId = ns("periodo"),
            label = "Período:",
            range = TRUE,
            addon = "none",
            view = "months",
            minView = "months",
            dateFormat = "MM/yyyy",
            minDate = as.Date("2024-01-01"),
            maxDate = as.Date("2024-12-31"),
            value = c(
              as.Date("2024-01-01"),
              as.Date("2024-12-01")
            ),
            autoClose = TRUE,
            clearButton = TRUE,
            language = "pt",
            width = "100%"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        card(
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Diagrama de Pareto por Área Selecionada"
          ),
          echarts4rOutput(ns("diag_pareto"))
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        card(
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Comparativo Mensal"
          ),
          echarts4rOutput(ns("comp_mes"))
        )
      ),
      column(
        width = 6,
        card(
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Comparativo por Período Fracionado"
          ),
          fluidRow(
            column(
              width = 9,
              echarts4rOutput(ns("comp_mes_per"))
              
            ),
            column(
              width = 3,
              div(
                class = "input-painel",
                selectInput(
                  ns("fracionados"),
                  "Período",
                  c(
                    "Bimestre" = "bim",
                    "Trimestre" = "trim",
                    "Quadrimestre" = "quad",
                    "Semestre" = "sem"
                  ),
                  width = "100%"
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        card(
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Resgistros Mensais"
          ),
          fluidRow(
            column(
              width = 2,
              div(
                class = "input-painel",
                pickerInput(
                  inputId = ns("meses_registros"),
                  label = "Meses",
                  choices = c("Jan","Fev","Mar","Abr","Mai","Jun",
                              "Jul","Ago","Set","Out","Nov","Dez"),
                  selected = c("Jan","Fev","Mar","Abr","Mai","Jun",
                               "Jul","Ago","Set","Out","Nov","Dez"),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `select-all-text` = "Todas",
                    `deselect-all-text` = "Nenhum",
                    `none-selected-text` = "Selecione uma opção",
                    `selected-text-format` = "count > 11",
                    `count-selected-text` = "Todos os Meses",
                    `live-search` = FALSE,
                    size = 6,
                    `container` = "body"
                  )
                )
              ),
              downloadButton(ns("baixar_base"), "Baixar base")
            ),
            column(
              width = 10,
              reactableOutput(ns("reg_mes"))
            )
          )
        )
      )
    )
  )
}

aba2_server <- function(id, base) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    base_filtrada <- reactive({
      
      req(
        input$titulo,
        input$area,
        input$tipo,
        input$periodo
      )
      
      base %>%
        filter(
          descricao_do == input$titulo,
          .data[[input$area]] %in% input$tipo
        )
    })
    
    output$diag_pareto <- renderEcharts4r({
      
      req(
        input$titulo,
        input$tipo,
        input$periodo[1],
        input$periodo[2]
      )
      
      col_pref <- switch(input$area,
                           "cisp" = "aisp",
                           "aisp" = "risp",
                           NULL
      )

      pref_area <- if (input$area %in% c("risp", "aisp", "cisp")) {
        paste0(toupper(input$area), " ")
      } else {
        ""
      }
      
      dados_graf3 <- base_filtrada() %>%
        filter(mes %in% month(input$periodo[1]):month(input$periodo[2]))
      
      if (!is.null(col_pref)) {
        dados_graf3 <- dados_graf3 %>%
          group_by(.data[[input$area]], .data[[col_pref]]) %>%
          summarise(n = n(), .groups = "drop") %>%
          mutate(
            texto_pref = paste0("<br/>", toupper(col_pref), " ", .data[[col_pref]])
          )
      } else {
        dados_graf3 <- dados_graf3 %>%
          count(.data[[input$area]]) %>%
          mutate(texto_pref = "") 
      }
      
      dados_graf3 %>%
        arrange(desc(n)) %>%
        mutate(
          perc = n / sum(n) * 100,
          perc_acum = cumsum(perc)
        ) %>%
        e_charts_(input$area) %>%
        e_bar(n, name = "Observado", bind = texto_pref, y_index = 0) %>%
        e_line(
          perc,
          name = "Percentual",
          symbol = "none",
          lineStyle = list(opacity = 0)
        ) %>% 
        e_line(
          perc_acum, 
          name = "Percentual Acumulado", 
          y_index = 1,
          symbol = "circle",
          lineStyle = list(color = "#f4b400"),
          itemStyle = list(color = "#f4b400")
        ) %>%
        e_x_axis(
          axisLabel = list(color = "#080b81", fontWeight = "bold"),
          axisTick = list(show = FALSE)
        ) %>% 
        e_y_axis(
          index = 0,
          axisLabel = list(
            color = "#080b81", fontWeight = "bold",
            formatter = htmlwidgets::JS("function(value) { return value.toLocaleString('pt-BR'); }")
          )
        ) %>%
        e_y_axis(
          index = 1, min = 0, max = 100, interval = 20,
          axisLabel = list(color = "#080b81", fontWeight = "bold", formatter = "{value}%")
        ) %>%
        e_color(c("#080b81", "#f4b400")) %>%
        e_legend(show = FALSE) %>%
        e_tooltip(
          trigger = "axis",
          formatter = htmlwidgets::JS(paste0("
            function(params) {
              // params[0].axisValue é a categoria original (ex: 12)
              // params[0].name agora contém o texto do 'bind' (ex: ' - AISP 4')
              
              var prefixo = '", pref_area, "';
              var categoria = params[0].axisValue; 
              var pai = params[0].name; 
              
              // Se o pai for undefined ou igual à categoria (segurança), deixa vazio
              if (!pai || pai === categoria) { pai = ''; }
    
              var titulo = '<b>' + prefixo + categoria + pai + '</b><br/>';
              
              var f = new Intl.NumberFormat('pt-BR', { minimumFractionDigits: 1, maximumFractionDigits: 1 });
              var res = titulo;
              
              params.forEach(function(item) {
                var valor = item.value[1];
                
                if (item.seriesName === 'Observado') {
                  res += 'Observado: ' + parseInt(valor).toLocaleString('pt-BR') + '<br/>';
                } else if (item.seriesName === 'Percentual') {
                  res += 'Percentual: ' + f.format(valor) + '%<br/>';
                } else if (item.seriesName === 'Percentual Acumulado') {
                  res += 'Percentual Acumulado: ' + f.format(valor) + '%';
                }
              });
              return res;
            }
          "))
        ) %>% 
        e_toolbox_feature("saveAsImage") %>% 
        e_text_g(
          left = "45%",
          top = "30%", 
          style = list(
            text = "ISP",
            fontSize = 125,
            fill = "rgba(0, 0, 0, 0.05)",
            z = -10             
          )
        )
    }) %>% 
      bindEvent(input$titulo, input$tipo, input$periodo)
    
    output$comp_mes <- renderEcharts4r({
      
      req(
        input$titulo,
        input$tipo,
        input$periodo[1],
        input$periodo[2]
      )

      base_filtrada() %>% 
        count(mes) %>%
        mutate(mes = case_when(
          mes == "1" ~ "Jan",
          mes == "2" ~ "Fev",
          mes == "3" ~ "Mar",
          mes == "4" ~ "Abr",
          mes == "5" ~ "Mai",
          mes == "6" ~ "Jun",
          mes == "7" ~ "Jul",
          mes == "8" ~ "Ago",
          mes == "9" ~ "Set",
          mes == "10" ~ "Out",
          mes == "11" ~ "Nov",
          mes == "12" ~ "Dez"
        )) %>%
        e_charts(mes) %>%
        e_line(n, name = "Observado") %>%
        e_x_axis(
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold"
          ),
          axisTick = list(show = FALSE)
        ) %>% 
        e_legend(show = FALSE) %>%
        e_y_axis(
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold",
            formatter = htmlwidgets::JS("
              function(value) {
                return value.toLocaleString('pt-BR');
              }
            ")
          )
        ) %>%
        e_color("#08618f") %>%
        e_tooltip(
          trigger = "axis",
          formatter = htmlwidgets::JS("
            function(params){
              
              var categoria = params[0].name;
              var valor = params[0].value[1];
              
              var formatado = new Intl.NumberFormat('pt-BR', {
                minimumFractionDigits: 0,
                maximumFractionDigits: 0
              }).format(valor);
              
              return '<b>' + categoria + '</b><br/>' + 'Observado: ' + formatado;
            }
          ")
        ) %>% 
        e_toolbox_feature("saveAsImage") %>% 
        e_text_g(
          left = "42%",
          top = "30%", 
          style = list(
            text = "ISP",
            fontSize = 125,
            fill = "rgba(0, 0, 0, 0.05)",
            z = -10             
          )
        )
    }) %>% 
      bindEvent(input$titulo, input$tipo, input$periodo)

    output$comp_mes_per <- renderEcharts4r({
      
      req(
        input$titulo,
        input$tipo,
        input$periodo[1],
        input$periodo[2]
      )

      base_filtrada() %>% 
        count(.data[[input$fracionados]]) %>%
        e_charts_(input$fracionados) %>%
        e_line(n, name = "Observado") %>%
        e_x_axis(
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold"
          ),
          axisTick = list(show = FALSE)
        ) %>% 
        e_legend(show = FALSE) %>%
        e_y_axis(
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold",
            formatter = htmlwidgets::JS("
              function(value) {
                return value.toLocaleString('pt-BR');
              }
            ")
          )
        ) %>%
        e_color("#08618f") %>%
        e_tooltip(
          trigger = "axis",
          formatter = htmlwidgets::JS("
            function(params){
              
              var categoria = params[0].name;
              var valor = params[0].value[1];
              
              var formatado = new Intl.NumberFormat('pt-BR', {
                minimumFractionDigits: 0,
                maximumFractionDigits: 0
              }).format(valor);
              
              return '<b>' + categoria + '</b><br/>' + 'Observado: ' + formatado;
            }
          ")
        ) %>% 
        e_toolbox_feature("saveAsImage") %>% 
        e_text_g(
          left = "38%",
          top = "30%", 
          style = list(
            text = "ISP",
            fontSize = 125,
            fill = "rgba(0, 0, 0, 0.05)",
            z = -10             
          )
        )
    }) %>% 
      bindEvent(input$titulo, input$tipo, input$periodo, input$fracionados)

    output$reg_mes <- renderReactable({
      
      req(
        input$titulo,
        input$tipo,
        input$periodo[1],
        input$periodo[2]
      )
      
      ordem_meses <- c(
        "Ano",
        "Jan","Fev","Mar","Abr","Mai","Jun",
        "Jul","Ago","Set","Out","Nov","Dez",
        "Total"
      )
      
      base_filtrada() %>% 
        count(mes) %>%
        mutate(mes = case_when(
          mes == "1" ~ "Jan",
          mes == "2" ~ "Fev",
          mes == "3" ~ "Mar",
          mes == "4" ~ "Abr",
          mes == "5" ~ "Mai",
          mes == "6" ~ "Jun",
          mes == "7" ~ "Jul",
          mes == "8" ~ "Ago",
          mes == "9" ~ "Set",
          mes == "10" ~ "Out",
          mes == "11" ~ "Nov",
          mes == "12" ~ "Dez"
        )) %>%
        filter(mes %in% input$meses_registros) %>%
        add_row(mes = "Total", n = sum(.$n)) %>% 
        add_row(mes = "Ano", n = 2024) %>% 
        mutate(mes = factor(mes, levels = ordem_meses)) %>%
        arrange(mes) %>% 
        pivot_wider(names_from = mes, values_from = n) %>%
        reactable(
          defaultColDef = colDef(
            width = 100,
            align = "center", 
            headerStyle = list(
              textAlign = "center",
              fontWeight = "bold"
            ),
            format = colFormat(
              separators = TRUE,
              digits = 0,
              locales = "pt-BR"
            )
          ),
          columns = list(
            Ano = colDef(
              name = "Ano",
              width = 80,
              align = "left",
              headerStyle = list(
                textAlign = "left",
                fontWeight = "bold"
              ),
              format = colFormat(separators = FALSE) 
            ),
            Total = colDef(
              style = list(fontWeight = "bold")
            )
          ),
          style = list(
            backgroundColor = "#080b8100",
            color = "#080b81"
          )
        )
    }) %>% 
      bindEvent(input$titulo, input$tipo, input$periodo, input$meses_registros)

    observeEvent(input$area, {

      labels_area <- case_when(
        input$area == "estado" ~ "Estado",
        input$area == "municípios" ~ "Município",
        input$area == "regiao_administrativa" ~ "Região Administrativa",
        input$area == "risp" ~ "RISP",
        input$area == "aisp" ~ "AISP",
        input$area == "cisp" ~ "CISP"
      )
      
      escolhas2 <- base %>% 
        select(.data[[input$area]]) %>%
        distinct() %>% 
        pull() %>% 
        str_sort(numeric = TRUE)
      
      total_opcoes2 <- length(escolhas2)

      updatePickerInput(
        session = session,
        inputId = "tipo",
        label = labels_area,
        choices = escolhas2,
        selected = escolhas2,
        options = list(
          `selected-text-format` = paste0("count > ", total_opcoes2 - 1),
          `count-selected-text` = "Todos"
        )
      )

    })

    output$baixar_base <- downloadHandler(
      filename = function() {
        
        texto_download <- switch(
          input$area,
          "estado" = "Estado",
          "municípios" = "Municípios",
          "regiao_administrativa" = "Região Administrativa",
          "risp" = "RISP",
          "aisp" = "AISP",
          "cisp" = "CISP"
        )
        
        paste0("ISP - ", input$titulo, " - ", texto_download, " - ", Sys.Date(), ".csv")
      },
      content = function(file) {
        
        ordem_meses <- c(
          "Ano",
          "Jan","Fev","Mar","Abr","Mai","Jun",
          "Jul","Ago","Set","Out","Nov","Dez",
          "Total"
        )
        
        dados <- base_filtrada() %>% 
          count(mes) %>%
          mutate(mes = case_when(
            mes == "1" ~ "Jan",
            mes == "2" ~ "Fev",
            mes == "3" ~ "Mar",
            mes == "4" ~ "Abr",
            mes == "5" ~ "Mai",
            mes == "6" ~ "Jun",
            mes == "7" ~ "Jul",
            mes == "8" ~ "Ago",
            mes == "9" ~ "Set",
            mes == "10" ~ "Out",
            mes == "11" ~ "Nov",
            mes == "12" ~ "Dez"
          )) %>%
          filter(mes %in% input$meses_registros) %>%
          add_row(mes = "Total", n = sum(.$n)) %>% 
          add_row(mes = "Ano", n = 2024) %>% 
          mutate(mes = factor(mes, levels = ordem_meses)) %>%
          arrange(mes) %>% 
          pivot_wider(names_from = mes, values_from = n)

        write.csv2(dados, file, row.names = FALSE, fileEncoding = "latin1")
      }
    )

  })
}