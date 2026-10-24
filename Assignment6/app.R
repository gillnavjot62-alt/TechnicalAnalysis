# BDA400 - Assignment 6
# Technical Analysis using R, Visualization Phase
# Student: Navjot Singh
#
# This Shiny dashboard:
# - fetches Yahoo Finance stock data
# - supports Daily, Weekly, and Monthly views
# - supports Line, Candlestick, and Area charts
# - overlays SMA, EMA, Bollinger Bands, RSI, and MACD
# - generates Buy/Sell/Hold signals using moving-average crossovers
# - annotates Buy/Sell events on the price chart
#
# Required packages:
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr", "xts"))

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)
library(xts)

safe_get_stock <- function(symbol, from, to) {
  tryCatch(
    getSymbols(symbol, src = "yahoo", from = from, to = to,
               auto.assign = FALSE, warnings = FALSE),
    error = function(e) {
      stop(paste("Unable to download", symbol, "from Yahoo Finance:", e$message))
    }
  )
}

aggregate_stock <- function(x, timeframe) {
  if (timeframe == "Weekly") {
    return(to.weekly(x, indexAt = "endof", drop.time = TRUE))
  }
  if (timeframe == "Monthly") {
    return(to.monthly(x, indexAt = "endof", drop.time = TRUE))
  }
  x
}

to_price_df <- function(x) {
  data.frame(
    Date = as.Date(index(x)),
    Open = as.numeric(Op(x)),
    High = as.numeric(Hi(x)),
    Low = as.numeric(Lo(x)),
    Close = as.numeric(Cl(x)),
    Volume = as.numeric(Vo(x)),
    Adjusted = as.numeric(Ad(x))
  )
}

ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", value = "AAPL"),
      dateRangeInput(
        "date_range", "Select Date Range",
        start = Sys.Date() - 365, end = Sys.Date(),
        max = Sys.Date()
      ),
      selectInput(
        "time_frame", "Select Time Frame",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type", "Chart Type",
        choices = c("Line", "Candlestick", "Area"),
        selected = "Candlestick"
      ),
      checkboxGroupInput(
        "technical_indicators", "Technical Indicators",
        choices = c(
          "SMA 20/50" = "SMA",
          "EMA 20" = "EMA",
          "Bollinger Bands" = "BBands",
          "RSI" = "RSI",
          "MACD" = "MACD"
        ),
        selected = c("SMA")
      ),
      sliderInput("short_ma", "Short MA Period", min = 5, max = 40, value = 20),
      sliderInput("long_ma", "Long MA Period", min = 20, max = 100, value = 50),
      actionButton("refresh", "Refresh Data"),
      hr(),
      helpText("Trading rule: Buy when the short SMA crosses above the long SMA; Sell when it crosses below.")
    ),
    mainPanel(
      h4(textOutput("status")),
      plotOutput("stock_chart", height = "500px"),
      conditionalPanel(
        condition = "input.technical_indicators.includes('RSI')",
        plotOutput("rsi_chart", height = "220px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.includes('MACD')",
        plotOutput("macd_chart", height = "250px")
      ),
      h4("Latest Trading Signal"),
      tableOutput("signal_table")
    )
  )
)

server <- function(input, output, session) {

  raw_data <- eventReactive(input$refresh, {
    req(input$symbol, input$date_range)
    validate(
      need(input$date_range[1] < input$date_range[2],
           "Start date must be earlier than end date.")
    )
    safe_get_stock(
      toupper(trimws(input$symbol)),
      as.Date(input$date_range[1]),
      as.Date(input$date_range[2]) + 1
    )
  }, ignoreNULL = FALSE)

  stock_data <- reactive({
    aggregate_stock(raw_data(), input$time_frame)
  })

  analysis_data <- reactive({
    x <- stock_data()
    df <- to_price_df(x)

    validate(
      need(nrow(df) >= input$long_ma,
           paste("Choose a wider date range. At least", input$long_ma,
                 "observations are needed for the selected long MA."))
    )

    close_prices <- df$Close
    df$SMA_Short <- SMA(close_prices, n = input$short_ma)
    df$SMA_Long <- SMA(close_prices, n = input$long_ma)
    df$EMA20 <- EMA(close_prices, n = min(20, max(2, nrow(df) - 1)))

    bb_n <- min(20, max(2, nrow(df) - 1))
    bb <- BBands(HLC(x), n = bb_n)
    df$BB_Down <- as.numeric(bb[, "dn"])
    df$BB_Mid <- as.numeric(bb[, "mavg"])
    df$BB_Up <- as.numeric(bb[, "up"])

    rsi_n <- min(14, max(2, nrow(df) - 1))
    df$RSI <- as.numeric(RSI(close_prices, n = rsi_n))

    macd_fast <- min(12, max(2, nrow(df) - 2))
    macd_slow <- min(26, max(macd_fast + 1, nrow(df) - 1))
    macd_signal <- min(9, max(2, nrow(df) - macd_slow))
    if (macd_slow < nrow(df) && macd_signal >= 2) {
      m <- MACD(close_prices, nFast = macd_fast,
                nSlow = macd_slow, nSig = macd_signal, percent = FALSE)
      df$MACD <- as.numeric(m[, "macd"])
      df$MACD_Signal <- as.numeric(m[, "signal"])
      df$MACD_Hist <- df$MACD - df$MACD_Signal
    } else {
      df$MACD <- NA_real_
      df$MACD_Signal <- NA_real_
      df$MACD_Hist <- NA_real_
    }

    # Trading signals based on actual crossover events.
    prev_short <- dplyr::lag(df$SMA_Short)
    prev_long <- dplyr::lag(df$SMA_Long)

    df$Signal <- "Hold"
    buy_idx <- !is.na(prev_short) & !is.na(prev_long) &
      prev_short <= prev_long & df$SMA_Short > df$SMA_Long
    sell_idx <- !is.na(prev_short) & !is.na(prev_long) &
      prev_short >= prev_long & df$SMA_Short < df$SMA_Long

    df$Signal[buy_idx] <- "Buy"
    df$Signal[sell_idx] <- "Sell"
    df
  })

  output$status <- renderText({
    paste(
      toupper(trimws(input$symbol)), "-",
      input$time_frame, input$chart_type, "view -",
      format(input$date_range[1]), "to", format(input$date_range[2])
    )
  })

  output$stock_chart <- renderPlot({
    df <- analysis_data()

    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), linewidth = 0.7)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), alpha = 0.25) +
        geom_line(aes(y = Close), linewidth = 0.6)
    } else {
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High), linewidth = 0.35) +
        geom_rect(
          aes(
            xmin = Date - 0.35, xmax = Date + 0.35,
            ymin = pmin(Open, Close), ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          linewidth = 0.2
        ) +
        scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "grey40"),
                          guide = "none")
    }

    if ("SMA" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = SMA_Short, linetype = "Short SMA"), linewidth = 0.7) +
        geom_line(aes(y = SMA_Long, linetype = "Long SMA"), linewidth = 0.7)
    }

    if ("EMA" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = EMA20, linetype = "EMA 20"), linewidth = 0.65)
    }

    if ("BBands" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = BB_Up), linewidth = 0.45, linetype = "dashed") +
        geom_line(aes(y = BB_Mid), linewidth = 0.45, linetype = "dotted") +
        geom_line(aes(y = BB_Down), linewidth = 0.45, linetype = "dashed")
    }

    events <- df[df$Signal != "Hold", ]
    if (nrow(events) > 0) {
      p <- p +
        geom_point(data = events, aes(y = Close, shape = Signal), size = 3) +
        geom_text(
          data = events,
          aes(y = Close, label = Signal),
          vjust = ifelse(events$Signal == "Buy", -0.8, 1.5),
          size = 3.5
        )
    }

    p +
      labs(
        title = paste(toupper(trimws(input$symbol)), "Price and Trading Signals"),
        x = NULL, y = "Price", linetype = "Indicator", shape = "Signal"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$rsi_chart <- renderPlot({
    df <- analysis_data()
    ggplot(df, aes(Date, RSI)) +
      geom_line(linewidth = 0.7) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "Relative Strength Index (RSI)", x = NULL, y = "RSI") +
      theme_minimal(base_size = 11)
  })

  output$macd_chart <- renderPlot({
    df <- analysis_data()
    ggplot(df, aes(Date)) +
      geom_col(aes(y = MACD_Hist), alpha = 0.45) +
      geom_line(aes(y = MACD, linetype = "MACD"), linewidth = 0.7) +
      geom_line(aes(y = MACD_Signal, linetype = "Signal"), linewidth = 0.7) +
      labs(title = "MACD", x = NULL, y = "Value", linetype = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
  })

  output$signal_table <- renderTable({
    df <- analysis_data()
    latest <- tail(df, 1)
    data.frame(
      Symbol = toupper(trimws(input$symbol)),
      Date = latest$Date,
      Close = round(latest$Close, 2),
      Short_MA = round(latest$SMA_Short, 2),
      Long_MA = round(latest$SMA_Long, 2),
      Signal = latest$Signal
    )
  })
}

shinyApp(ui, server)
