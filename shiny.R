library(shiny)
library(ggplot2)
library(tidyverse)
library(tsibble)
library(tsME918)

ui <- fluidPage(
  titlePanel("Modelos de Previsão de Séries Temporiais"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Adicione um arquivo:",
                accept = ".csv"),
      HTML("<p style='font-size:small;'> * O arquivo deve ser em formato .csv e conter as colunas Year, Month, Make e Quantity, escritas exatamente dessa forma </p>"),
      numericInput("npred", "Meses de previsão desejados:", value = 3, min = 1, max = 10),
      selectInput("make", "Categorias", choices = NULL),
      selectInput("models", "Modelos", choices = NULL),
      br(),
      numericInput("p", "p:", value = NULL),
      numericInput("d", "d:", value = NULL),
      numericInput("q", "q:", value = NULL),
      numericInput("P", "P:", value = NULL),
      numericInput("D", "D:", value = NULL),
      numericInput("Q", "Q:", value = NULL),
      numericInput("S", "S:", value = NULL),
      checkboxInput("prev", "Previsões", FALSE),
      checkboxInput("metrics", "Métricas", FALSE)
    ),

    mainPanel(
      fluidRow(
        column(6, plotOutput("ACF")),
        column(6, plotOutput("PACF"))
      ),
      plotOutput("ST"),
      fluidRow(
        column(6, conditionalPanel(
          condition = "input.prev == 1",
          tableOutput("prev")
        )),
        column(6, conditionalPanel(
          condition = "input.metrics == 1",
          tableOutput("metrics")
        ))
      )
    )
  )
)

# Definir servidor
server <- function(input, output, session) {

  dados <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })

  dadoslimpos <- reactive({
    tryCatch(
      {
        cleaning(dados())
      },
      error = function(e) {
        showNotification(
          paste(e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
    )
  })

  observe({
    if (input$models == "SARIMA") {
      updateNumericInput(session, "P", value = 1)
      updateNumericInput(session, "Q", value = 1)
      updateNumericInput(session, "D", value = 1)
      updateNumericInput(session, "S", value = 1)
    }
  })

  observe({
    updateSelectInput(session, "make",
                      choices = unique(dadoslimpos()$Make))
    updateSelectInput(session, "models",
                      choices = c("AUTOMÁTICO", "ARIMA", "SARIMA"))
  })

  dadosfiltrados <- reactive({
    req(input$make)
    dadoslimpos() %>%
      filter(Make == input$make)
  })

  dadosparametros <- reactive({
    if (input$models == "ARIMA") {
      p <- input$p
      d <- input$d
      q <- input$q
      P <- 0
      Q <- 0
      D <- 0
      S <- 0
      npred <- input$npred
      params <- data.frame(p = p,
                           d = d,
                           q = q,
                           P = P,
                           D = D,
                           Q = Q,
                           S = S,
                           npred = npred)
      params
    } else if (input$models == "SARIMA") {
      p <- input$p
      q <- input$q
      d <- input$d
      P <- input$P
      Q <- input$Q
      D <- input$D
      S <- input$S
      npred <- input$npred
      params <- data.frame(p = p,
                           d = d,
                           q = q,
                           P = P,
                           D = D,
                           Q = Q,
                           S = S,
                           npred = npred)
      params
    } else {
      p <- 0
      q <- 0
      d <- 0
      P <- 0
      Q <- 0
      D <- 0
      S <- 0
      npred <- input$npred
      params <- data.frame(p = p,
                           d = d,
                           q = q,
                           P = P,
                           D = D,
                           Q = Q,
                           S = S,
                           npred = npred)
      params
    }
  })

  dadosmodelados <- reactive({
    if (input$models == "ARIMA") {
      modeling(dadosfiltrados(),
               "ARIMA",
               dadosparametros()$p,
               dadosparametros()$d,
               dadosparametros()$q,
               dadosparametros()$P,
               dadosparametros()$D,
               dadosparametros()$Q,
               dadosparametros()$S,
               dadosparametros()$npred)
    } else if (input$models == "SARIMA") {
      modeling(dadosfiltrados(),
               "SARIMA",
               dadosparametros()$p,
               dadosparametros()$d,
               dadosparametros()$q,
               dadosparametros()$P,
               dadosparametros()$D,
               dadosparametros()$Q,
               dadosparametros()$S,
               dadosparametros()$npred)
    } else {
      modeling(dadosfiltrados(),
               "AUTO",
               dadosparametros()$p,
               dadosparametros()$d,
               dadosparametros()$q,
               dadosparametros()$P,
               dadosparametros()$D,
               dadosparametros()$Q,
               dadosparametros()$S,
               dadosparametros()$npred)
    }

  })

  dadoscompletos <- reactive({
    if (input$models == "ARIMA") {
      dadosmodelados() %>%
        select(-TestPredictionSarima, -PredictionSarima,
               -TestPredictionAuto, -PredictionAuto)
    } else if (input$models == "SARIMA") {
      dadosmodelados() %>%
        select(-TestPredictionArima, -PredictionArima,
               -TestPredictionAuto, -PredictionAuto)
    } else {
      dadosmodelados() %>%
        select(-TestPredictionArima, -PredictionArima,
               -TestPredictionSarima, -PredictionSarima)
    }
  })

  observe({
    if (input$models == "AUTOMÁTICO") {
      modelo <- dadoscompletos()$Models[[1]][3,2]
      parâmetros <- as.numeric(str_extract_all(modelo, "\\d+")[[1]])

      updateNumericInput(session, "p", value = parâmetros[1])
      updateNumericInput(session, "d", value = parâmetros[2])
      updateNumericInput(session, "q", value = parâmetros[3])
    }
  })

  output$ACF <- renderPlot({

    ts_series <- dadoscompletos()$Series[[1]] %>%
      select(Date, Quantity) %>%
      arrange(Date) %>%
      as_tsibble(index = Date, regular = TRUE)

    acf_result <- acf(ts_series$Quantity, na.action = na.pass)

    df_acf <- data.frame(lag = seq_along(acf_result$acf) - 1,
                         acf = as.numeric(acf_result$acf))

    ggplot(df_acf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0)) +
      labs(title = "Função de Autocorrelação (ACF)",
           x = "Lag",
           y = "Autocorrelação") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })

  output$PACF <- renderPlot({

    ts_series <- dadoscompletos()$Series[[1]] %>%
      select(Date, Quantity) %>%
      arrange(Date) %>%
      as_tsibble(index = Date, regular = TRUE)

    pacf_result <- acf(ts_series$Quantity, type = "partial")

    df_pacf <- data.frame(lag = seq_along(pacf_result$acf) - 1,
                          pacf = as.numeric(pacf_result$acf))

    ggplot(df_pacf, mapping = aes(x = lag, y = pacf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0)) +
      labs(title = "Função de Autocorrelação Parcial (PACF)",
           x = "Lag",
           y = "Autocorrelação Parcial") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })

  output$ST <- renderPlot({
    if ("TestPredictionArima" %in% colnames(dadoscompletos())) {
      detalhes <- dadoscompletos()$Models[[1]][1,2]
      testprediction <- dadoscompletos()$TestPredictionArima[[1]]
      prediction <- dadoscompletos()$PredictionArima[[1]]
    } else if ("TestPredictionSarima" %in% colnames(dadoscompletos())) {
      detalhes <- dadoscompletos()$Models[[1]][2,2]
      testprediction <- dadoscompletos()$TestPredictionSarima[[1]]
      prediction <- dadoscompletos()$PredictionSarima[[1]]
    } else {
      detalhes <- dadoscompletos()$Models[[1]][3,2]
      testprediction <- dadoscompletos()$TestPredictionAuto[[1]]
      prediction <- dadoscompletos()$PredictionAuto[[1]]
    }

    series <- dadoscompletos()$Series[[1]] %>%
      select(Date, Quantity) %>%
      rename(Value = Quantity) %>%
      mutate(Group = "Series")

    testprediction <- testprediction %>%
      select(Date, TestPred) %>%
      rename(Value = TestPred) %>%
      mutate(Group = "TestPred")

    prediction <- prediction %>%
      select(Date, Pred) %>%
      rename(Value = Pred) %>%
      mutate(Group = "Pred")

    dadosgrafico <- bind_rows(series, testprediction, prediction)

    print(dadosgrafico)

    ggplot(mapping = aes(x = Date, y = Value),
           data = dadosgrafico) +
      geom_line(aes(color = Group)) +
      geom_point(aes(color = Group)) +
      labs(x = "Data", y = "Quantidade", color = "") +
      ggtitle(detalhes) +
      scale_color_manual(
        values = c("Series" = "black",
                   "TestPred" = "#619CFF",
                   "Pred" = "#00BA38"),
        labels = c("Series" = "Série",
                   "TestPred" = "Previsão de Teste",
                   "Pred" = "Previsão")
      ) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "top",
            legend.justification = c(1, 0))
  })

  output$prev <- renderTable({
    if ("TestPredictionArima" %in% colnames(dadoscompletos())) {
      dadosprev <- dadoscompletos()$PredictionArima[[1]] %>%
        mutate(Data = as.character(Date),
               Previsão = Pred) %>%
        select(Data, Previsão)
      dadosprev
    } else if ("TestPredictionSarima" %in% colnames(dadoscompletos())) {
      dadosprev <- dadoscompletos()$PredictionSarima[[1]] %>%
        mutate(Data = as.character(Date),
               Previsão = Pred) %>%
        select(Data, Previsão)
      dadosprev
    } else {
      dadosprev <- dadoscompletos()$PredictionAuto[[1]] %>%
        mutate(Data = as.character(Date),
               Previsão = Pred) %>%
        select(Data, Previsão)
      dadosprev
    }
  })

  output$metrics <- renderTable({
    if (input$metrics) {
      if (input$models == "ARIMA") {
        dadoscompletos()$Models[[1]][1,]
      } else if (input$models == "SARIMA") {
        dadoscompletos()$Models[[1]][2,]
      } else {
        dadoscompletos()$Models[[1]][3,]
      }
    } else {
      NULL
    }
  })
 }

shinyApp(ui, server)

