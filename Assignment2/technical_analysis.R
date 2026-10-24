# BDA400 Assignment 2 - Technical Analysis using R, Preliminary Stage
# Student: Navjot Singh

# Run these once if the packages are not installed:
# install.packages(c("quantmod", "TTR"))

library(quantmod)
library(TTR)

# Read symbols from portfolio.txt and download daily market data from Yahoo Finance.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]
  if (length(symbols) == 0) stop("portfolio.txt does not contain any stock symbols.")

  stock_list <- list()
  for (symbol in symbols) {
    message("Downloading: ", symbol)
    stock_list[[symbol]] <- tryCatch(
      getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        warning(paste("Could not download", symbol, "-", conditionMessage(e)))
        NULL
      }
    )
  }
  stock_list[!vapply(stock_list, is.null, logical(1))]
}

# Statistical mode helper. For continuous prices, exact repeats may be uncommon.
stat_mode <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Calculate requested statistics using adjusted closing prices.
calculate_statistics <- function(stock_data, ma_period = 20) {
  results <- list()
  for (symbol in names(stock_data)) {
    adjusted <- as.numeric(Ad(stock_data[[symbol]]))
    adjusted <- adjusted[is.finite(adjusted)]
    ma <- SMA(adjusted, n = ma_period)

    results[[symbol]] <- data.frame(
      Symbol = symbol,
      Observations = length(adjusted),
      Mean = mean(adjusted, na.rm = TRUE),
      Mode = stat_mode(adjusted),
      Median = median(adjusted, na.rm = TRUE),
      Standard_Deviation = sd(adjusted, na.rm = TRUE),
      Latest_MA20 = tail(na.omit(ma), 1),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, results)
}

# Display the first and last rows of every downloaded stock data set.
display_stock_data <- function(stock_data) {
  for (symbol in names(stock_data)) {
    cat("\n==============================\n")
    cat("Stock:", symbol, "\n")
    cat("First rows:\n")
    print(head(stock_data[[symbol]]))
    cat("Last rows:\n")
    print(tail(stock_data[[symbol]]))
  }
}

# Plot adjusted closing price and 20-day simple moving average for every stock.
plot_stock_data <- function(stock_data, ma_period = 20) {
  for (symbol in names(stock_data)) {
    adjusted_xts <- Ad(stock_data[[symbol]])
    ma_xts <- SMA(adjusted_xts, n = ma_period)
    chartSeries(
      stock_data[[symbol]],
      name = paste(symbol, "Daily Stock Data"),
      theme = chartTheme("white"),
      TA = NULL
    )
    addSMA(n = ma_period, col = "blue")
  }
}

# Main workflow
stocks <- load_stock_data("portfolio.txt")
display_stock_data(stocks)
statistics <- calculate_statistics(stocks)
print(statistics)
plot_stock_data(stocks)
