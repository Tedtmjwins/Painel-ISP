aba1_ui <- function(id, base) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        div(
          class = "input-painel",
          selectInput(
            ns("titulo3"),
            "Título",
            base %>% distinct(descricao_do) %>%
              arrange(
                descricao_do == "Outros",
                descricao_do
              ) %>% pull(),
            width = "100%"
          )
        )
      ),
      column(
        width = 3,
        div(
          class = "input-painel",
          selectInput(
            ns("area3"),
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
            inputId = ns("tipo3"),
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
          selectInput(
            ns("serie"),
            "Série",
            c("Mensal", "Acumulado"),
            "Mensal"
          )
        )
      ),
      column(
        width = 2,
        uiOutput(ns("periodo3_ui"))
      )
    ),
    fluidRow(
      column(
        width = 4,
        uiOutput(ns("value_box_1"))
      ),
      column(
        width = 4,
        uiOutput(ns("value_box_2"))
      ),
      column(
        width = 4,
        uiOutput(ns("value_box_3"))
      )
    ),
    fluidRow(
      column(
        width = 6,
        fluidRow(
          column(
            width = 12,
            card(
              class = "card-painel",
              card_title(
                class = "card-titulo",
                "Ranking das Áreas com mais Registros"
              ),
              echarts4rOutput(ns("rank_top"), height = "250px")
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
                "Sexo das Vítimas"
              ),
              div(
                style = "height: 250px",
                uiOutput(ns("graf_sexo"))
              )
            )
          ),
          column(
            width = 6,
            card(
              class = "card-painel",
              card_title(
                class = "card-titulo",
                "Faixa Etária das Vítimas"
              ),
              div(
                style = "height: 250px",
                uiOutput(ns("fx_etaria"))
              )
            )
          )
        )
      ),
      column(
        width = 6,
        card(
          class = "card-painel",
          card_title(
            class = "card-titulo",
            "Distribuição Espacial"
          ),
          leafletOutput(ns("mapa"), height = "600px")
        )
      )
    )
  )
}

aba1_server <- function(id, base, estado, municípios, regiao_administrativa, risp,
                        aisp, cisp) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    base_filtrada3 <- reactive({
      
      req(
        input$titulo3,
        input$area3,
        input$tipo3,
        input$periodo3
      )
      
      base %>%
        filter(
          descricao_do == input$titulo3,
          .data[[input$area3]] %in% input$tipo3,
          mes %in% meses()
        )
    })
    
    meses <- reactive({
      
      if (input$serie == "Mensal") {
        
        lubridate::month(input$periodo3)
        
      } else {
        
        req(length(input$periodo3) == 2)
        
        seq(
          lubridate::month(input$periodo3[1]),
          lubridate::month(input$periodo3[2])
        )
      }
      
    }) %>% 
      bindEvent(input$titulo3, input$tipo3, input$periodo3)
    
    output$periodo3_ui <- renderUI({
      
      if (input$serie == "Mensal") {
        
        div(
          style = "font-size: 18px; color: #000a63;
                       font-weight: bold;",
          airDatepickerInput(
            inputId = ns("periodo3"),
            label = "Período:",
            addon = "none",
            range = FALSE,
            view = "months",
            minView = "months",
            dateFormat = "MM/yyyy",
            minDate = as.Date("2024-01-01"),
            maxDate = as.Date("2024-12-31"),
            value = as.Date("2024-01-01"),
            autoClose = TRUE,
            clearButton = TRUE,
            language = "pt"
          )
        )
        
      } else {
        
        div(
          style = "font-size: 18px; color: #000a63;
                       font-weight: bold;",
          airDatepickerInput(
            inputId = ns("periodo3"),
            label = "Período:",
            addon = "none",
            range = TRUE,
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
            language = "pt"
          )
        )
        
      }
    })
    
    output$graf_sexo <- renderUI({

      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )

      dados_graf1 <- base_filtrada3() %>%
        count(sexo) %>%
        filter(sexo %in% c("1", "2")) %>%
        mutate(sexo = ifelse(sexo == "1", "Feminino", "Masculino"))

      if (nrow(dados_graf1) == 0) {

        h5("Sem Registros", style = "color:#000000;")

      } else {

        dados_graf1 %>%
          e_charts(sexo, height = "250px") %>%
          e_bar(n, name = "Sexo") %>%
          e_x_axis(
            axisLabel = list(
              color = "#080b81",
              fontWeight = "bold"
            ),
            axisTick = list(show = FALSE)
          ) %>%
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
          e_legend(show = F) %>%
          e_grid(top = -20, right = 20, bottom = 30, left = 50) %>%
          e_color("#29065a") %>%
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
            left = "35%",
            top = "25%",
            style = list(
              text = "ISP",
              fontSize = 125,
              fill = "rgba(0, 0, 0, 0.05)",
              z = -10
            )
          )

      }
    }) %>%
      bindEvent(input$titulo3, input$tipo3, input$periodo3)

    output$rank_top <- renderEcharts4r({

      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )

      base_filtrada3() %>%
        count(.data[[input$area3]]) %>%
        arrange(-n) %>%
        slice(1:5) %>%
        arrange(n) %>%
        mutate(
          !!sym(input$area3) := if (input$area3 %in% c("risp", "aisp", "cisp")) {
            paste(str_to_upper(input$area3), .data[[input$area3]])
          } else {
            .data[[input$area3]]
          }
        ) %>%
        e_charts_(input$area3) %>%
        e_bar(n, name = "Ranking") %>%
        e_x_axis(
          axisLabel = list(
            color = "#080b81",
            fontWeight = "bold"
          ),
          axisTick = list(show = FALSE)
        ) %>%
        e_legend(show = F) %>%
        e_grid(left = 200, top = 0, bottom = 0) %>%
        e_color("#08618f") %>%
        e_flip_coords() %>%
        e_tooltip(
          trigger = "axis",
          formatter = htmlwidgets::JS("
            function(params){

              var categoria = params[0].name;
              var valor = params[0].value[0];

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
          left = "50%",
          top = "30%",
          style = list(
            text = "ISP",
            fontSize = 125,
            fill = "rgba(0, 0, 0, 0.05)",
            z = -10
          )
        )

    }) %>%
      bindEvent(input$titulo3, input$tipo3, input$periodo3)

    output$fx_etaria <- renderUI({

      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )

      dados_graf2 <- base_filtrada3() %>%
        drop_na(idade) %>%
        mutate(fx_etaria = case_when(
          idade %in% c(60:150) ~ "Mais de 60 anos",
          idade %in% c(45:59) ~ "45 a 59 anos",
          idade %in% c(35:44) ~ "35 a 44 anos",
          idade %in% c(25:34) ~ "25 a 34 anos",
          idade %in% c(18:24) ~ "18 a 24 anos",
          idade %in% c(12:17) ~ "12 a 17 anos",
          idade %in% c(0:11) ~ "0 a 11 anos"
        )) %>%
        count(fx_etaria) %>%
        arrange(desc(fx_etaria))

      if (nrow(dados_graf2) == 0) {

        h5("Sem Registros", style = "color:#000000;")

      } else {

        dados_graf2 %>%
        e_charts(fx_etaria, height = "250px") %>%
          e_bar(n, name = "Faixa Etária") %>%
          e_x_axis(
            axisLabel = list(
              color = "#080b81",
              fontWeight = "bold"
            ),
            axisTick = list(show = FALSE)
          ) %>%
          e_flip_coords() %>%
          e_legend(show = F) %>%
          e_color("#145343") %>%
          e_grid(left = 110, top = 0, bottom = 0) %>%
          e_tooltip(
            trigger = "axis",
            formatter = htmlwidgets::JS("
            function(params){

              var categoria = params[0].name;
              var valor = params[0].value[0];

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
            left = "35%",
            top = "25%",
            style = list(
              text = "ISP",
              fontSize = 125,
              fill = "rgba(0, 0, 0, 0.05)",
              z = -10
            )
          )

      }
    }) %>%
      bindEvent(input$titulo3, input$tipo3, input$periodo3)
    
    output$value_box_1 <- renderUI({
      
      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )
      
      value_box_func(
        "Casos Registrados",
        base_filtrada3() %>% count() %>% pull(),
        "clipboard-list",
        "#08618f"
      )

    }) %>% 
      bindEvent(input$titulo3, input$tipo3, input$periodo3)
    
    output$value_box_2 <- renderUI({
      
      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )
      
      value_box_func(
        "Casos com Sexo das Vítimas Registrados",
        base_filtrada3() %>% filter(sexo %in% c("1", "2")) %>% count() %>% pull(),
        "venus-mars",
        "#29065a"
      )
      
    }) %>% 
      bindEvent(input$titulo3, input$tipo3, input$periodo3)
    
    output$value_box_3 <- renderUI({
      
      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )
      
      value_box_func(
        "Casos com Idade das Vítimas Registrados",
        base_filtrada3() %>% drop_na(idade) %>% count() %>% pull(),
        "image-portrait",
        "#145343"
      )

    }) %>% 
      bindEvent(input$titulo3, input$tipo3, input$periodo3)

    output$mapa <- renderLeaflet({

      req(
        input$titulo3,
        input$tipo3,
        input$periodo3
      )

      mapa_base <- switch(
        input$area3,
        "estado" = estado,
        "municípios" = municípios,
        "regiao_administrativa" = regiao_administrativa,
        "risp" = risp,
        "aisp" = aisp,
        "cisp" = cisp
      )

      dados_mapa <- left_join(
        mapa_base,
        base_filtrada3() %>%
          count(.data[[input$area3]]),
        by = paste0(input$area3)
      ) %>%
        mutate(
          n = replace_na(n, 0),
          !!sym(input$area3) := if (input$area3 %in% c("risp", "aisp", "cisp")) {
            paste(str_to_upper(input$area3), .data[[input$area3]])
          } else {
            .data[[input$area3]]
          }
        )

      valores_positivos <- dados_mapa$n[dados_mapa$n > 0]

      if (length(valores_positivos) > 0) {
        min_val <- min(valores_positivos, na.rm = TRUE)
        max_val <- max(valores_positivos, na.rm = TRUE)

        v_domain <- log1p(c(min_val, max_val))

        pal_gradual <- colorNumeric(
          palette = viridis(256, option = "plasma", direction = -1),
          domain = v_domain,
          na.color = "#e6eeff"
        )
      } else {
        pal_gradual <- function(x) "#e6eeff"
        v_domain <- c(0, 1)
      }
      
      suppressWarnings({
        dados_mapa %>%
          leaflet(options = leafletOptions(zoomSnap = 0.25)) %>%
          htmlwidgets::onRender("
          function(el, x) {
            el.style.background = '#ffffff00';
          }
        ") %>%
          addPolygons(
            color = "#808182",
            weight = 1,
            label = ~lapply(
              paste0(
                "<b style='font-size:16px;'>", get(input$area3), "</b><br/>",
                "<span style='font-size:14px;'>Observado: ",
                format(n, big.mark = ".", decimal.mark = ","), "</span>"
              ),
              htmltools::HTML
            ),
            fillColor = ~ifelse(
              n == 0,
              "#e6eeff",
              pal_gradual(log1p(n))
            ),
            fillOpacity = 1,
            highlightOptions = highlightOptions(
              weight = 2,
              color = "black",
              bringToFront = TRUE
            )
          ) %>%
          addLegend(
            pal = pal_gradual,
            values = v_domain,
            title = "Observações",
            position = "bottomright",
            labFormat = labelFormat(
              transform = function(x) round(expm1(x)),
              digits = 0,
              big.mark = "."
            )
          )
      })
    }) %>%
      bindEvent(input$titulo3, input$tipo3, input$periodo3)
    
    
    observeEvent(input$area3, {
  
      labels_area3 <- case_when(
        input$area3 == "estado" ~ "Estado",
        input$area3 == "municípios" ~ "Município",
        input$area3 == "regiao_administrativa" ~ "Região Administrativa",
        input$area3 == "risp" ~ "RISP",
        input$area3 == "aisp" ~ "AISP",
        input$area3 == "cisp" ~ "CISP"
      )
      
      escolhas1 <- base %>% 
        select(.data[[input$area3]]) %>%
        distinct() %>% 
        pull() %>% 
        str_sort(numeric = TRUE)
      
      total_opcoes1 <- length(escolhas1)
      
      updatePickerInput(
        session = session,
        inputId = "tipo3",
        label = labels_area3,
        choices = escolhas1,
        selected = escolhas1,
        options = list(
          `selected-text-format` = paste0("count > ", total_opcoes1 - 1),
          `count-selected-text` = "Todos"
        )
      )
    })
    
  })
}
