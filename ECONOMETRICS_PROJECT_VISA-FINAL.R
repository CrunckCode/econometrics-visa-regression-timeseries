#############################################
# A) MULTIVARIATE LINEAR REGRESSION
#############################################

# 1. Load packages
library(car)
library(lmtest)
library(whitestrap)
library(memisc)
library(tseries)
library(fGarch)
library(openxlsx)
library(corrplot)
library(dplyr)

# 2. Read the Excel file
df <- as.data.frame(read.xlsx("VISA_Final_Dataset_Clean.xlsx"))
df$date <- as.Date(df$date)

# 3. Correlation Heatmap
df_num <- df %>% dplyr::select(where(is.numeric))
corr_matrix <- cor(df_num)

corrplot(
  corr_matrix,
  method = "color",
  type = "full",
  tl.cex = 0.55,
  col = colorRampPalette(c("darkblue","white","darkred"))(200),
  title = "Figure A: Correlation Heatmap of VISA Predictors",
  mar = c(0,0,2,0)
)

# 4. Full OLS Model
model_full <- lm(
  VISA_Price ~ VISA_Volume +
    SP500_Return + Nasdaq_Return + VIX_Level +
    DXY_Return + Gold_Return + Bitcoin_Return +
    Oil_Price + Retail_Sales + PCE_Consumption + Industrial_Production +
    Unemp_Rate + Personal_Income +
    Consumer_Sentiment + Consumer_Loans +
    CPI_Index + CPI_Mom +
    Y10Y + Y2Y +
    Financial_Conditions_Index +
    Payroll_Employment + Real_Disposable_Income +
    Housing_Starts,
  data = df
)

coef(model_full)
summary(model_full)

# 5. Checking the Gauss Markov assumptions
plot(model_full, which = 1, col = "darkblue", sub = "",
     main = "Figure B1: Residuals vs Fitted Values for Full Model")

plot(model_full, which = 2, col = "black", sub = "",
     main = "Figure B2: QQ Plot of Standardized Residuals for Full Model")
vif(model_full)
car::crPlots(model_full, ylab = "Residuals",
             main = "Figure B3: Residual Plots for all independent variables")

dwtest(model_full)                                 
sum(residuals(model_full))                        
bptest(model_full)                                 
white_test(model_full)
               
y  <- df$VISA_Price
yh <- fitted(model_full)
SST_full <- sum((y - mean(y))^2)
SSR_full <- deviance(model_full)
SSE_full <- SST_full - SSR_full
SSR_full

# 6. Reduced Model
model_visa_reduced <- lm(
  VISA_Price ~
    SP500_Return + Nasdaq_Return + VIX_Level +
    Oil_Price +
    PCE_Consumption + Unemp_Rate +
    Y2Y +
    Financial_Conditions_Index +
    Payroll_Employment +
    Real_Disposable_Income +
    Consumer_Loans,
  data = df
)

summary(model_visa_reduced)
SSR_red <- deviance(model_visa_reduced)
SSR_red

# 7. Final Model
model_visa_final <- lm(
  VISA_Price ~
    PCE_Consumption +
    Unemp_Rate +
    Y2Y +
    Financial_Conditions_Index +
    Payroll_Employment,
  data = df
)

coef(model_visa_final)
residuals(model_visa_final)
summary(model_visa_final)

SSR_final <- deviance(model_visa_final)
SSR_final
y  <- df$VISA_Price
yhf <- fitted(model_visa_final)

# 8. Checking Gauss Markov assumptions again on the Final Model
plot(model_visa_final, which = 1, col = "darkblue", sub = "",
     main = "Figure C1: Residuals vs Fitted Values for Final Model")

plot(model_visa_final, which = 2, col = "black", sub = "",
     main = "Figure C2: QQ Plot of Standardized Residuals for Final Model")

vif(model_visa_final)
car::crPlots(model_visa_final, ylab = "Residuals",
             main = "Figure C3: Component plus Residual Plots for Final Model")

dwtest(model_visa_final)
sum(residuals(model_visa_final))
bptest(model_visa_final)
white_test(model_visa_final)

# 9. Model Comparison Table
mtable(
  "Full Model"    = model_full,
  "Reduced Model" = model_visa_reduced,
  "Final Model"   = model_visa_final
)

#############################################
# B) TIME SERIES SECTION – VISA
#############################################

# 1. Convert VISA monthly price to a time series
visa_prices_ts <- ts(df$VISA_Price, start = c(2020, 1), frequency = 12)

plot(visa_prices_ts, type = "l",
     main = "Figure D1: VISA Monthly Adjusted Price Series",
     xlab = "Time", ylab = "Price")

adf.test(visa_prices_ts)
kpss.test(visa_prices_ts)

# 2. Convert to log returns and split into training/testing
visa_logret_ts <- diff(log(visa_prices_ts))
visa_logret_ts <- na.omit(visa_logret_ts)

n_train <- 52
train_ts <- window(visa_logret_ts, end = time(visa_logret_ts)[n_train])
test_ts  <- window(visa_logret_ts, start = time(visa_logret_ts)[n_train + 1])

length(train_ts)
length(test_ts)

# 3. Stationarity of Training Set
plot(visa_logret_ts, type = "l",
     main = "Figure D2: VISA Monthly Log Returns", 
     xlab = "Time", ylab = "Log Return")

adf.test(visa_logret_ts)
kpss.test(visa_logret_ts)

# 4. AR(1)
ar1_visa <- arima(train_ts, order = c(1, 0, 0))
ar1_visa

# 5. ARMA(1,1)
arma11_visa <- arima(train_ts, order = c(1, 0, 1))
arma11_visa

# 6. ARIMA(1,1,1)
arima111_visa <- arima(train_ts, order = c(1, 1, 1))
arima111_visa

# 7. ARMA(1,1) + GARCH(1,1)
garch_visa <- garchFit(~ arma(1, 1) + garch(1, 1), data = train_ts,
                       cond.dist = "norm")
garch_visa

# 8. Forecasts
ar1_pred       <- predict(ar1_visa,       n.ahead = length(test_ts))$pred
arma11_pred    <- predict(arma11_visa,    n.ahead = length(test_ts))$pred
arima111_pred  <- predict(arima111_visa,  n.ahead = length(test_ts))$pred
garch_pred     <- predict(garch_visa,     n.ahead = length(test_ts))$meanForecast
actual         <- as.numeric(test_ts)

# 9. Forecast-based SSR
SSR_ar1_forecast      <- sum((ar1_pred      - actual)^2)
SSR_arma11_forecast   <- sum((arma11_pred   - actual)^2)
SSR_arima111_forecast <- sum((arima111_pred - actual)^2)
SSR_garch_forecast    <- sum((garch_pred    - actual)^2)

# 10. Final Comparison Table
SSR_forecast_comparison <- data.frame(
  Model = c("AR(1)", "ARMA(1,1)", "ARIMA(1,1,1)", "ARMA(1,1) + GARCH(1,1)"),
  Forecast_SSR = c(
    SSR_ar1_forecast,
    SSR_arma11_forecast,
    SSR_arima111_forecast,
    SSR_garch_forecast
  )
)

print(SSR_forecast_comparison)