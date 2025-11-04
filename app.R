#========================================= APLICATIVO SHINY =======================================
# Aplicativo para análise de correlação entre Precipitação e Leptospirose em MG
# Com persistência de dados entre navegação de abas

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(DT)

#========================================= INTERFACE DO USUÁRIO ===================================

ui <- navbarPage("Análise Leptospirose - MG (2012-2023)",
                 
  # Aba 1: Dados do Paciente
  tabPanel("Dados do Paciente",
    fluidPage(
      titlePanel("Entrada de Dados do Paciente"),
      
      sidebarLayout(
        sidebarPanel(
          h4("Informações do Paciente"),
          
          textInput("id_paciente", "ID do Paciente:", ""),
          
          dateInput("data_notificacao", "Data de Notificação:", 
                   value = Sys.Date(), format = "dd/mm/yyyy"),
          
          selectInput("mesorregiao", "Mesorregião:",
                     choices = c("Selecione..." = "",
                               "Campo das Vertentes" = "CV",
                               "Central Mineira" = "CM",
                               "Jequitinhonha" = "Jeq",
                               "Metropolitana de Belo Horizonte" = "MBH",
                               "Noroeste de Minas" = "NoMG",
                               "Norte de Minas" = "NMG",
                               "Oeste de Minas" = "OMG",
                               "Sul/Sudoeste de Minas" = "SSeMG",
                               "Triângulo Mineiro" = "TrMG",
                               "Vale do Mucuri" = "VM",
                               "Vale do Rio Doce" = "VRD",
                               "Zona da Mata" = "ZM")),
          
          numericInput("idade", "Idade:", value = 0, min = 0, max = 120),
          
          selectInput("sexo", "Sexo:",
                     choices = c("Selecione..." = "",
                               "Masculino" = "M",
                               "Feminino" = "F")),
          
          textInput("municipio", "Município de Residência:", ""),
          
          hr(),
          
          # Botão Calcular
          actionButton("btn_calcular", "Calcular e Salvar Dados", 
                      class = "btn-primary btn-lg", 
                      style = "width: 100%;"),
          
          br(), br(),
          
          helpText("Clique em 'Calcular e Salvar Dados' para preservar as informações",
                  "antes de navegar para outras abas.")
        ),
        
        mainPanel(
          h3("Status dos Dados"),
          
          conditionalPanel(
            condition = "output.dados_salvos",
            div(class = "alert alert-success",
                icon("check-circle"),
                "Dados salvos com sucesso! Você pode navegar entre as abas."
            )
          ),
          
          conditionalPanel(
            condition = "!output.dados_salvos",
            div(class = "alert alert-info",
                icon("info-circle"),
                "Preencha os campos e clique em 'Calcular e Salvar Dados'."
            )
          ),
          
          br(),
          
          h4("Resumo dos Dados Inseridos:"),
          verbatimTextOutput("resumo_dados"),
          
          br(),
          
          h4("Data de Referência (Data de Notificação - 14 dias):"),
          verbatimTextOutput("data_referencia")
        )
      )
    )
  ),
  
  # Aba 2: Análise de Dados
  tabPanel("Análise de Dados",
    fluidPage(
      titlePanel("Análise dos Dados Salvos"),
      
      conditionalPanel(
        condition = "!output.dados_salvos",
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            h4("Atenção!"),
            p("Nenhum dado foi salvo ainda. Por favor, volte para a aba 'Dados do Paciente',",
              "preencha os campos e clique em 'Calcular e Salvar Dados'.")
        )
      ),
      
      conditionalPanel(
        condition = "output.dados_salvos",
        fluidRow(
          column(6,
            h3("Dados do Paciente Salvos"),
            tableOutput("tabela_paciente")
          ),
          column(6,
            h3("Informações Calculadas"),
            tableOutput("tabela_calculos")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(12,
            h3("Histórico de Pacientes"),
            DTOutput("tabela_historico")
          )
        )
      )
    )
  ),
  
  # Aba 3: Visualizações
  tabPanel("Visualizações",
    fluidPage(
      titlePanel("Visualizações e Gráficos"),
      
      conditionalPanel(
        condition = "!output.dados_salvos",
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            "Por favor, insira e salve dados na aba 'Dados do Paciente' primeiro."
        )
      ),
      
      conditionalPanel(
        condition = "output.dados_salvos",
        fluidRow(
          column(12,
            h3("Distribuição por Mesorregião"),
            plotOutput("grafico_mesorregiao", height = "400px")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(6,
            h3("Distribuição por Sexo"),
            plotOutput("grafico_sexo", height = "300px")
          ),
          column(6,
            h3("Distribuição por Faixa Etária"),
            plotOutput("grafico_idade", height = "300px")
          )
        )
      )
    )
  ),
  
  # Aba 4: Sobre
  tabPanel("Sobre",
    fluidPage(
      titlePanel("Sobre o Sistema"),
      
      h3("Análise da Correlação entre Precipitação e Incidência de Leptospirose em Minas Gerais (2012-2023)"),
      
      p("Este aplicativo permite a entrada, armazenamento e análise de dados de pacientes",
        "com leptospirose no estado de Minas Gerais."),
      
      h4("Funcionalidades:"),
      tags$ul(
        tags$li("Entrada de dados do paciente com persistência entre abas"),
        tags$li("Cálculo automático da data de referência (período de incubação de 14 dias)"),
        tags$li("Visualização e análise dos dados salvos"),
        tags$li("Histórico completo de pacientes cadastrados"),
        tags$li("Gráficos de distribuição por mesorregião, sexo e idade")
      ),
      
      h4("Como Usar:"),
      tags$ol(
        tags$li("Acesse a aba 'Dados do Paciente'"),
        tags$li("Preencha todos os campos do formulário"),
        tags$li("Clique no botão 'Calcular e Salvar Dados'"),
        tags$li("Os dados serão preservados ao navegar entre as abas"),
        tags$li("Visualize as análises nas abas 'Análise de Dados' e 'Visualizações'")
      ),
      
      hr(),
      
      h4("Informações do Projeto:"),
      p(strong("Estudo:"), "Correlação entre precipitação pluviométrica e incidência de leptospirose",
        "nas mesorregiões de Minas Gerais, 2012-2023"),
      p(strong("Autor:"), "Miguel Antonio Janssen - d202120265@uftm.edu.br"),
      p(strong("Orientadora:"), "Profa. Dra. Ana Paula Fernandes - anapaula.fernandes@uftm.edu.br")
    )
  )
)

#========================================= LÓGICA DO SERVIDOR =====================================

server <- function(input, output, session) {
  
  # ReactiveValues para armazenar dados persistentes
  dados_paciente <- reactiveValues(
    salvos = FALSE,
    id = NULL,
    data_notif = NULL,
    mesorregiao = NULL,
    idade = NULL,
    sexo = NULL,
    municipio = NULL,
    data_ref = NULL,
    historico = data.frame(
      ID = character(),
      Data_Notificacao = character(),
      Data_Referencia = character(),
      Mesorregiao = character(),
      Idade = numeric(),
      Sexo = character(),
      Municipio = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Observador do botão Calcular
  observeEvent(input$btn_calcular, {
    
    # Validação dos campos
    req(input$id_paciente, input$data_notificacao, input$mesorregiao,
        input$idade, input$sexo, input$municipio)
    
    if(input$id_paciente == "" || input$mesorregiao == "" || 
       input$sexo == "" || input$municipio == "") {
      showNotification("Por favor, preencha todos os campos!", 
                      type = "error", duration = 5)
      return()
    }
    
    # Calcular data de referência (14 dias antes da notificação)
    data_ref <- input$data_notificacao - ddays(14)
    
    # Salvar dados no reactiveValues
    dados_paciente$salvos <- TRUE
    dados_paciente$id <- input$id_paciente
    dados_paciente$data_notif <- input$data_notificacao
    dados_paciente$mesorregiao <- input$mesorregiao
    dados_paciente$idade <- input$idade
    dados_paciente$sexo <- input$sexo
    dados_paciente$municipio <- input$municipio
    dados_paciente$data_ref <- data_ref
    
    # Adicionar ao histórico
    novo_registro <- data.frame(
      ID = input$id_paciente,
      Data_Notificacao = as.character(input$data_notificacao),
      Data_Referencia = as.character(data_ref),
      Mesorregiao = input$mesorregiao,
      Idade = input$idade,
      Sexo = input$sexo,
      Municipio = input$municipio,
      stringsAsFactors = FALSE
    )
    
    dados_paciente$historico <- rbind(dados_paciente$historico, novo_registro)
    
    showNotification("Dados salvos com sucesso!", 
                    type = "message", duration = 3)
  })
  
  # Output: Indicador de dados salvos
  output$dados_salvos <- reactive({
    dados_paciente$salvos
  })
  outputOptions(output, "dados_salvos", suspendWhenHidden = FALSE)
  
  # Output: Resumo dos dados inseridos
  output$resumo_dados <- renderPrint({
    if(!dados_paciente$salvos) {
      cat("Nenhum dado salvo ainda.\n")
    } else {
      cat("ID do Paciente:", dados_paciente$id, "\n")
      cat("Data de Notificação:", as.character(dados_paciente$data_notif), "\n")
      cat("Mesorregião:", dados_paciente$mesorregiao, "\n")
      cat("Idade:", dados_paciente$idade, "anos\n")
      cat("Sexo:", dados_paciente$sexo, "\n")
      cat("Município:", dados_paciente$municipio, "\n")
    }
  })
  
  # Output: Data de referência
  output$data_referencia <- renderPrint({
    if(!dados_paciente$salvos) {
      cat("Nenhuma data calculada ainda.\n")
    } else {
      cat("Data de Referência:", as.character(dados_paciente$data_ref), "\n")
      cat("(Data de Notificação - 14 dias para período de incubação)")
    }
  })
  
  # Output: Tabela do paciente
  output$tabela_paciente <- renderTable({
    req(dados_paciente$salvos)
    
    data.frame(
      Campo = c("ID", "Data Notificação", "Mesorregião", "Idade", "Sexo", "Município"),
      Valor = c(dados_paciente$id,
               as.character(dados_paciente$data_notif),
               dados_paciente$mesorregiao,
               paste(dados_paciente$idade, "anos"),
               dados_paciente$sexo,
               dados_paciente$municipio)
    )
  })
  
  # Output: Tabela de cálculos
  output$tabela_calculos <- renderTable({
    req(dados_paciente$salvos)
    
    data.frame(
      Cálculo = c("Data de Referência", "Período de Incubação", "Ano", "Mês"),
      Resultado = c(as.character(dados_paciente$data_ref),
                   "14 dias",
                   format(dados_paciente$data_ref, "%Y"),
                   format(dados_paciente$data_ref, "%B"))
    )
  })
  
  # Output: Histórico de pacientes
  output$tabela_historico <- renderDT({
    req(nrow(dados_paciente$historico) > 0)
    
    datatable(dados_paciente$historico,
             options = list(pageLength = 10, scrollX = TRUE),
             rownames = FALSE)
  })
  
  # Output: Gráfico por mesorregião
  output$grafico_mesorregiao <- renderPlot({
    req(nrow(dados_paciente$historico) > 0)
    
    dados_paciente$historico %>%
      count(Mesorregiao) %>%
      ggplot(aes(x = reorder(Mesorregiao, n), y = n, fill = Mesorregiao)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Número de Casos por Mesorregião",
           x = "Mesorregião",
           y = "Número de Casos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Output: Gráfico por sexo
  output$grafico_sexo <- renderPlot({
    req(nrow(dados_paciente$historico) > 0)
    
    dados_paciente$historico %>%
      count(Sexo) %>%
      ggplot(aes(x = Sexo, y = n, fill = Sexo)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribuição por Sexo",
           x = "Sexo",
           y = "Número de Casos") +
      theme_minimal() +
      scale_fill_manual(values = c("M" = "#3498db", "F" = "#e74c3c"))
  })
  
  # Output: Gráfico por idade
  output$grafico_idade <- renderPlot({
    req(nrow(dados_paciente$historico) > 0)
    
    dados_paciente$historico %>%
      mutate(Faixa_Etaria = cut(Idade, 
                                breaks = c(0, 18, 30, 45, 60, 120),
                                labels = c("0-17", "18-29", "30-44", "45-59", "60+"))) %>%
      count(Faixa_Etaria) %>%
      ggplot(aes(x = Faixa_Etaria, y = n, fill = Faixa_Etaria)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribuição por Faixa Etária",
           x = "Faixa Etária",
           y = "Número de Casos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

#========================================= EXECUTAR APLICATIVO ====================================

shinyApp(ui = ui, server = server)
