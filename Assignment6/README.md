# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase

Student: Navjot Singh
Course: BDA400
Assignment: A06

## Project
This R Shiny dashboard fetches historical stock data from Yahoo Finance and provides interactive visualization and technical analysis.

Features:
- Stock-symbol input
- Date-range filtering
- Daily, weekly, and monthly time frames
- Line, candlestick, and area charts
- Toggleable SMA, EMA, Bollinger Bands, RSI, and MACD
- Customizable short and long moving-average periods
- Buy/Sell/Hold trading logic based on moving-average crossovers
- Buy and Sell annotations on the price chart
- Error handling for invalid dates, short data ranges, and Yahoo Finance failures

## Run
1. Install R and RStudio.
2. Install packages once:
   install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr", "xts"))
3. Open app.R in RStudio.
4. Click Run App.
5. Enter a stock symbol such as AAPL, MSFT, GOOG, AMZN, or TSLA.
6. Select dates, time frame, chart type, indicators, and MA periods.
7. Click Refresh Data.

## Repository location
Upload this Assignment6 folder to:
https://github.com/gillnavjot62-alt/TechnicalAnalysis/tree/main/Assignment6
