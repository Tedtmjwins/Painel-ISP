rm(list = ls())
cat("\f")

library(tidyverse)
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(echarts4r)
library(openxlsx)
library(shinyWidgets)
library(reactable)
library(viridis)
library(htmltools)

source("Abas/Aba 1.R")
source("Abas/Aba 2.R")
source("Abas/Aba 3.R")
source("Abas/Aba 4.R")
source("Gráficos e Funções/Funções.R")

base <- readRDS("base.rds")
cisp <- readRDS("Mapas/CISP/cisp.rds")
aisp <- readRDS("Mapas/AISP/aisp.rds")
risp <- readRDS("Mapas/RISP/risp.rds")
estado <- readRDS("Mapas/Estado/Estado_RJ.rds") %>% st_transform(4326)
municípios <- readRDS("Mapas/Municípios/Municípios_RJ.rds") %>% st_transform(4326)
regiao_administrativa <- readRDS("Mapas/RAs/RAs_RJ.rds") %>% st_transform(4326)

ui <- function(id) {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "dash_styles.css"),
      tags$style(HTML("
        body {
          background: linear-gradient(180deg, #e8ebed, #0066a6);
          background-attachment: fixed;
          background-repeat: no-repeat;
          background-size: cover;
        }

        nav.navbar {
          background: #e8ebed00 !important;
          padding-top: 0px;
          padding-bottom: 0px;
        }
      "))
    ),
    page_navbar(
      title = tags$a(
        href = "https://ispconecta.rj.gov.br/",
        target = "_blank",
        tags$img(
          src = "isp.png",
          height = "80px",
          style = "margin-left: 0px; margin-top: 10px"
        )
      ),
      window_title = "Painel ISP",
      theme = bs_theme(
        bootswatch = "zephyr",
        bg = "#e8ebed",
        fg = "#000a63"
      ),
      nav_panel(
        "Distribuição Espacial",
        aba1_ui("aba_1", base)
      ),
      nav_panel(
        "Análise Mensal",
        aba2_ui("aba_2", base)
      ),
      nav_panel(
        "Monitoramento por Área",
        aba3_ui("aba_3", base)
      ),
      nav_panel(
        "Comparativo",
        aba4_ui("aba_4", base)
      )
    )
  )
}

server <- function(input, output, session) {
  
  ns <- session$ns
  
  aba1_server("aba_1", base, estado, municípios, regiao_administrativa, risp,
              aisp, cisp)
  
  aba2_server("aba_2", base)
  
  aba3_server("aba_3", base)
  
  aba4_server("aba_4", base)
  
}

shinyApp(ui, server)
