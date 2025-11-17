library(shiny)
library(quantmod)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(zoo)

ui <- fluidPage(
  titlePanel("📊 Previsioni ARIMA con data previsione target"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Ticker Yahoo Finance:", value = "AAPL"),
      dateRangeInput("daterange", "Seleziona intervallo storico:",
                     start = "2018-01-01", end = Sys.Date(),
                     min = "2000-01-01", max = Sys.Date()),
      selectInput("freq", "Frequenza:", choices = c("Giornaliera", "Mensile")),
      dateInput("forecast_date", "Data di previsione target:", value = as.Date("2025-12-31"),
                min = Sys.Date()),
      actionButton("go", "Esegui Analisi")
    ),
    mainPanel(
      plotOutput("forecastPlot"),
      plotOutput("residualsPlot"),
      verbatimTextOutput("modelSummary"),
      tableOutput("accuracyTable")
    )
  )
)

server <- function(input, output) {
  
  dati <- eventReactive(input$go, {
    symbol <- toupper(input$symbol)
    tryCatch({
      getSymbols(symbol, from = input$daterange[1], to = input$daterange[2], auto.assign = FALSE)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  get_log_series <- function(prezzi, freq) {
    if (freq == "Mensile") {
      serie_xts <- log(to.monthly(prezzi, indexAt = "lastof", OHLC = FALSE))
      passo <- "1 month"
    } else {
      serie_xts <- log(prezzi)
      passo <- "1 day"
    }
    serie_xts <- na.omit(serie_xts)
    list(xts = serie_xts, passo = passo)
  }
  
  model_data <- reactive({
    req(dati())
    prezzi <- Ad(dati())
    log_data <- get_log_series(prezzi, input$freq)
    serie_xts <- log_data$xts
    passo <- log_data$passo
    
    n <- nrow(serie_xts)
    if (n < 20) return(NULL)  # minimo dati
    
    last_date <- index(serie_xts)[n]
    forecast_date <- input$forecast_date
    
    # calcolo h in base a frequenza e date
    if (input$freq == "Giornaliera") {
      h <- as.numeric(forecast_date - last_date)
    } else if (input$freq == "Mensile") {
      h <- 12 * (year(forecast_date) - year(last_date)) + (month(forecast_date) - month(last_date))
    } else {
      h <- 15  # default fallback
    }
    h <- max(h, 1)
    if (n <= h + 10) return(NULL)  # controllo dati sufficienti
    
    train <- serie_xts[1:(n - h)]
    test <- if (n > h) serie_xts[(n - h + 1):n] else NULL
    
    mod_arima <- auto.arima(coredata(train))
    fc <- forecast(mod_arima, h = h)
    
    future_dates <- seq(from = last_date + 1, by = passo, length.out = h)
    
    list(
      mod_arima = mod_arima,
      fc = fc,
      train = train,
      test = test,
      future_dates = future_dates,
      passo = passo
    )
  })
  
  output$forecastPlot <- renderPlot({
    md <- model_data()
    req(md)
    
    df_plot <- data.frame(
      Data = md$future_dates,
      Previsione = as.numeric(exp(md$fc$mean)),
      Lower = as.numeric(exp(md$fc$lower[,2])),
      Upper = as.numeric(exp(md$fc$upper[,2])),
      Reale = if(!is.null(md$test)) as.numeric(exp(md$test)) else NA
    )
    
    ggplot(df_plot, aes(x = Data)) +
      geom_line(aes(y = Previsione), color = "blue", size = 1.2) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
      geom_line(aes(y = Reale), color = "darkgreen", linetype = "dashed", size = 1.2, na.rm = TRUE) +
      labs(title = paste0("Previsione ARIMA (", input$freq, ") per ", toupper(input$symbol)),
           subtitle = "Blu = previsione | Verde = osservato",
           x = "Data", y = "Prezzo ($)") +
      theme_minimal()
  })
  
  output$residualsPlot <- renderPlot({
    md <- model_data()
    req(md)
    
    residui <- residuals(md$mod_arima)
    par(mfrow = c(1,2))
    Acf(residui, main = "ACF residui")
    Pacf(residui, main = "PACF residui")
    par(mfrow = c(1,1))
  })
  
  output$modelSummary <- renderPrint({
    md <- model_data()
    req(md)
    summary(md$mod_arima)
  })
  
  output$accuracyTable <- renderTable({
    md <- model_data()
    req(md)
    if (is.null(md$test)) return(NULL)
    round(accuracy(md$fc, coredata(md$test)), 4)
  })
  
}

shinyApp(ui = ui, server = server)
