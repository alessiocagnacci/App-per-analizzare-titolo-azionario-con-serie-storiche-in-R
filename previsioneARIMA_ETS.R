library(shiny)
library(quantmod)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(zoo)

ui <- fluidPage(
  titlePanel("📊 Previsioni ARIMA & ETS con data previsione target"),
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
    if (n < 20) return(NULL)
    
    last_date <- index(serie_xts)[n]
    forecast_date <- input$forecast_date
    
    if (input$freq == "Giornaliera") {
      h <- as.numeric(forecast_date - last_date)
    } else if (input$freq == "Mensile") {
      h <- 12 * (year(forecast_date) - year(last_date)) + (month(forecast_date) - month(last_date))
    } else {
      h <- 15
    }
    h <- max(h, 1)
    if (n <= h + 10) return(NULL)
    
    train <- serie_xts[1:(n - h)]
    test <- if (n > h) serie_xts[(n - h + 1):n] else NULL
    freq <- if (input$freq == "Mensile") 12 else 5
    
    # Modello ARIMA
    mod_arima <- auto.arima(ts(coredata(train), frequency = freq))
    fc_arima <- forecast(mod_arima, h = h)
    
    # Modello ETS
    mod_ets <- ets(ts(coredata(train), frequency = freq)) 
    fc_ets <- forecast(mod_ets, h = h)
    
    future_dates <- seq(from = last_date + 1, by = passo, length.out = h)
    
    list(
      mod_arima = mod_arima,
      fc_arima = fc_arima,
      mod_ets = mod_ets,
      fc_ets = fc_ets,
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
      Previsione_ARIMA = as.numeric(exp(md$fc_arima$mean)),
      Lower_ARIMA = as.numeric(exp(md$fc_arima$lower[,2])),
      Upper_ARIMA = as.numeric(exp(md$fc_arima$upper[,2])),
      Previsione_ETS = as.numeric(exp(md$fc_ets$mean)),
      Lower_ETS = as.numeric(exp(md$fc_ets$lower[,2])),
      Upper_ETS = as.numeric(exp(md$fc_ets$upper[,2])),
      Reale = if(!is.null(md$test)) as.numeric(exp(md$test)) else NA
    )
    
    ggplot(df_plot, aes(x = Data)) +
      geom_line(aes(y = Previsione_ARIMA, color = "ARIMA"), size = 1.2) +
      geom_ribbon(aes(ymin = Lower_ARIMA, ymax = Upper_ARIMA, fill = "ARIMA"), alpha = 0.2) +
      geom_line(aes(y = Previsione_ETS, color = "ETS"), size = 1.2) +
      geom_ribbon(aes(ymin = Lower_ETS, ymax = Upper_ETS, fill = "ETS"), alpha = 0.2) +
      geom_line(aes(y = Reale), color = "darkgreen", linetype = "dashed", size = 1.2, na.rm = TRUE) +
      scale_color_manual(name = "Modelli", values = c("ARIMA" = "blue", "ETS" = "red")) +
      scale_fill_manual(name = "Intervalli", values = c("ARIMA" = "lightblue", "ETS" = "pink")) +
      labs(title = paste0("Previsioni ARIMA vs ETS (", input$freq, ") per ", toupper(input$symbol)),
           subtitle = "Blu = ARIMA | Rosso = ETS | Verde = osservato",
           x = "Data", y = "Prezzo ($)") +
      theme_minimal()
  })
  
  output$residualsPlot <- renderPlot({
    md <- model_data()
    req(md)
    par(mfrow = c(2,2))
    Acf(residuals(md$mod_arima), main = "ACF residui ARIMA")
    Pacf(residuals(md$mod_arima), main = "PACF residui ARIMA")
    Acf(residuals(md$mod_ets), main = "ACF residui ETS")
    Pacf(residuals(md$mod_ets), main = "PACF residui ETS")
    par(mfrow = c(1,1))
  })
  
  output$modelSummary <- renderPrint({
    md <- model_data()
    req(md)
    cat("=== ARIMA ===\n")
    print(summary(md$mod_arima))
    cat("\n=== ETS ===\n")
    print(summary(md$mod_ets))
  })
  
  output$accuracyTable <- renderTable({
    md <- model_data()
    req(md)
    if (is.null(md$test)) return(NULL)
    acc_arima <- accuracy(md$fc_arima, coredata(md$test))
    acc_ets <- accuracy(md$fc_ets, coredata(md$test))
    data.frame(
      Modello = c("ARIMA", "ETS"),
      RMSE = c(acc_arima["Test set", "RMSE"], acc_ets["Test set", "RMSE"]),
      MAE = c(acc_arima["Test set", "MAE"], acc_ets["Test set", "MAE"]),
      MAPE = c(acc_arima["Test set", "MAPE"], acc_ets["Test set", "MAPE"])
    )
  })
  
}

shinyApp(ui = ui, server = server)
