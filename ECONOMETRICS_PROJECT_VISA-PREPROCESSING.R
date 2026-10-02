###########################################
# A) REGRESSION SECTION - VISA
###########################################

###########################################
# 1. Load packages
###########################################

library(quantmod)
library(tidyverse)
library(GGally)
library(lubridate)
library(zoo)
library(xts)
library(car)
library(lmtest)
library(whitestrap)
library(memisc)
library(tseries)
library(fGarch)
library(openxlsx)
library(sandwich)
library(corrplot)
library(dplyr)

###########################################
# 2. Download Data (Yahoo Finance + FRED)
###########################################

start_date <- as.Date("2019-12-01")
end_date   <- as.Date("2025-06-30")

# Yahoo Finance tickers for Visa and market/asset factors
tickers <- c("V","^GSPC","^IXIC","^VIX","DX-Y.NYB","GLD","BTC-USD")
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

# FRED series
fred_codes <- c(
  "RSAFS",      # Retail sales
  "PCE",        # Personal consumption expenditures
  "INDPRO",     # Industrial production
  "UNRATE",     # Unemployment rate
  "CPIAUCSL",   # CPI index
  "DGS10",      # 10 year Treasury
  "DGS2",       # 2 year Treasury
  "TOTCI",      # Consumer loans
  "NFCI",       # National Financial Conditions Index
  "PI",         # Personal income
  "UMCSENT",    # Consumer sentiment
  "DCOILWTICO", # WTI oil
  "PAYEMS",     # Payroll employment
  "DSPIC96",    # Real disposable income
  "HOUST"       # Housing starts
)

getSymbols(fred_codes, src = "FRED", from = start_date, to = end_date)


###########################################
# 3. Convert to Monthly
###########################################

# Visa and market prices (adjusted close monthly)
V_m    <- to.monthly(Ad(V),          indexAt = "lastof", OHLC = FALSE)
SPX_m  <- to.monthly(Ad(GSPC),       indexAt = "lastof", OHLC = FALSE)
IXIC_m <- to.monthly(Ad(IXIC),       indexAt = "lastof", OHLC = FALSE)
VIX_m  <- to.monthly(Ad(VIX),        indexAt = "lastof", OHLC = FALSE)
DXY_m  <- to.monthly(Ad(`DX-Y.NYB`), indexAt = "lastof", OHLC = FALSE)
GLD_m  <- to.monthly(Ad(GLD),        indexAt = "lastof", OHLC = FALSE)
BTC_m  <- to.monthly(Ad(`BTC-USD`),  indexAt = "lastof", OHLC = FALSE)

# Rename
colnames(V_m)    <- "VISA_Price"
colnames(SPX_m)  <- "SPX_Level"
colnames(IXIC_m) <- "Nasdaq_Level"
colnames(VIX_m)  <- "VIX_Level"
colnames(DXY_m)  <- "DXY_Level"
colnames(GLD_m)  <- "Gold_Level"
colnames(BTC_m)  <- "BTC_Level"

# Visa trading volume (monthly)
V_vol_m <- to.monthly(Vo(V), indexAt = "lastof", OHLC = FALSE)
colnames(V_vol_m) <- "VISA_Volume"

# FRED monthly series
Retail_m   <- to.monthly(RSAFS,      indexAt = "lastof", OHLC = FALSE)
PCE_m      <- to.monthly(PCE,        indexAt = "lastof", OHLC = FALSE)
IND_m      <- to.monthly(INDPRO,     indexAt = "lastof", OHLC = FALSE)
UNR_m      <- to.monthly(UNRATE,     indexAt = "lastof", OHLC = FALSE)
CPI_m      <- to.monthly(CPIAUCSL,   indexAt = "lastof", OHLC = FALSE)
Y10_m      <- to.monthly(DGS10,      indexAt = "lastof", OHLC = FALSE)
Y2_m       <- to.monthly(DGS2,       indexAt = "lastof", OHLC = FALSE)
Loans_m    <- to.monthly(TOTCI,      indexAt = "lastof", OHLC = FALSE)
NFCI_m     <- to.monthly(NFCI,       indexAt = "lastof", OHLC = FALSE)
Income_m   <- to.monthly(PI,         indexAt = "lastof", OHLC = FALSE)
Sent_m     <- to.monthly(UMCSENT,    indexAt = "lastof", OHLC = FALSE)
Oil_m      <- to.monthly(DCOILWTICO, indexAt = "lastof", OHLC = FALSE)
Payroll_m  <- to.monthly(PAYEMS,     indexAt = "lastof", OHLC = FALSE)
RDIncome_m <- to.monthly(DSPIC96,    indexAt = "lastof", OHLC = FALSE)
Housing_m  <- to.monthly(HOUST,      indexAt = "lastof", OHLC = FALSE)


###########################################
# 4. Derived Variables
###########################################

# Market and asset log returns
SPX_ret_raw   <- diff(log(SPX_m))
Nasdaq_ret_raw<- diff(log(IXIC_m))
DXY_ret_raw   <- diff(log(DXY_m))
Gold_ret_raw  <- diff(log(GLD_m))
BTC_ret_raw   <- diff(log(BTC_m))

# CPI month over month (percent)
CPI_Mom_raw   <- diff(log(CPI_m)) * 100

# Rename derived series
colnames(SPX_ret_raw)    <- "SP500_Return"
colnames(Nasdaq_ret_raw) <- "Nasdaq_Return"
colnames(DXY_ret_raw)    <- "DXY_Return"
colnames(Gold_ret_raw)   <- "Gold_Return"
colnames(BTC_ret_raw)    <- "Bitcoin_Return"
colnames(CPI_Mom_raw)    <- "CPI_Mom"

##################################################################
# 5. Master Dataset Construction and Correlation Heatmap Analysis
##################################################################
# 5A. Merge All Data to a Master Data Set
df_raw <- merge(
  V_m, V_vol_m,
  SPX_ret_raw, Nasdaq_ret_raw, VIX_m, DXY_ret_raw,
  Gold_ret_raw, BTC_ret_raw,
  Oil_m,
  Retail_m, PCE_m, IND_m, UNR_m,
  Income_m, Sent_m, Loans_m,
  CPI_m, CPI_Mom_raw, Y10_m, Y2_m,
  NFCI_m,
  Payroll_m, RDIncome_m, Housing_m
)

colnames(df_raw) <- c(
  "VISA_Price","VISA_Volume",
  "SP500_Return","Nasdaq_Return","VIX_Level","DXY_Return",
  "Gold_Return","Bitcoin_Return",
  "Oil_Price",
  "Retail_Sales","PCE_Consumption","Industrial_Production","Unemp_Rate",
  "Personal_Income","Consumer_Sentiment","Consumer_Loans",
  "CPI_Index","CPI_Mom","Y10Y","Y2Y",
  "Financial_Conditions_Index",
  "Payroll_Employment","Real_Disposable_Income","Housing_Starts"
)

df_xts <- na.omit(df_raw)
df <- data.frame(date = index(df_xts), coredata(df_xts))

