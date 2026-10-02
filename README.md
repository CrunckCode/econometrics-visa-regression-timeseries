# An Econometric Assessment of Visa Inc. (V) Share Price: Regression and Time Series

Solo final project for the Rutgers MQF Econometrics course (Prof. Mariya Naumova), November 2025, written in R. It asks what drives Visa's share price using a multivariable OLS regression on price levels, then models log returns with AR, ARMA, ARIMA and ARMA-GARCH and compares them out of sample.

## Files
- `ECONOMETRICS_PROJECT_VISA-PREPROCESSING.R`: pulls and assembles the monthly dataset.
- `ECONOMETRICS_PROJECT_VISA-FINAL.R`: regression, diagnostics, model selection and time-series models.
- `VISA_Final_Dataset_Clean.xlsx`: the cleaned monthly panel.
- `VISA_SSR_Comparison.xlsx`: out-of-sample SSR comparison across models.
- `An Econometric Assessment of Visa Inc Share Price - Regression and Time-Series.pdf`: the 52 page paper.

## Data
Monthly panel from December 2019 to June 2025 (n = 66), 24 variables. `quantmod` pulls market series from Yahoo Finance (V, S&P 500, Nasdaq, VIX, dollar index, gold, bitcoin) and 15 macro series from FRED (retail sales, PCE, industrial production, unemployment, CPI, 10Y and 2Y yields, consumer loans, NFCI, personal income, sentiment, WTI, payrolls, real disposable income, housing starts). Converted to end of month, with log-return transforms for market series.

## Part A: multivariable OLS on price levels
- Full model with 23 predictors: R2 0.9499, adjusted R2 0.9225, F = 34.62, SSR 8,148.
- Classical assumption battery: t-tests and F-test, VIF (severe multicollinearity, e.g. PCE about 2,496, CPI about 1,336), Durbin-Watson 1.2157 (positive autocorrelation, p = 1.6e-6), zero conditional mean check, Breusch-Pagan (p = 0.4714) and White (p = 0.120) heteroskedasticity tests, and Q-Q normality checks.
- Model reduction from 23 to 11 to 5 variables using t-statistics, VIF and economic relevance. Final model: Visa price on PCE consumption, unemployment rate, year-over-year growth, financial conditions index and payroll employment. R2 0.9105, adjusted R2 0.903, all five significant.
- The final model reintroduces problems: Durbin-Watson 0.90 and significant Breusch-Pagan (p = 0.0052) and White (p = 0.0015) tests, so HAC robust standard errors are recommended.

## Part B: time series on log returns
- Stationarity: price levels are non-stationary (ADF p = 0.876, KPSS p = 0.01); log returns are stationary (ADF p = 0.01, KPSS p = 0.10).
- 52 month training and 13 month test split. Fitted AR(1), ARMA(1,1), ARIMA(1,1,1) and ARMA(1,1)-GARCH(1,1) with `fGarch`. GARCH beta1 is about 0.994, meaning very persistent volatility.
- Out-of-sample SSR: AR(1) 0.02696 (best), ARMA(1,1) 0.02698, ARIMA(1,1,1) 0.02698, ARMA-GARCH 0.02702. The differences are tiny, so the simple AR(1) is preferred.

## Conclusion
Level regressions identify the macro drivers of Visa's price but are weakened by multicollinearity and residual autocorrelation. Time-series models on stationary log returns describe the short run more reliably.

## Limitations and next steps
Report Newey-West standard errors next to OLS, add a Chow or rolling stability check, extend GARCH to EGARCH/GJR with ARCH-LM and Ljung-Box tests, and replace the single 13 step forecast with a rolling one-step backtest.

## Requirements
R with `quantmod`, `car`, `lmtest`, `whitestrap`, `sandwich`, `tseries`, `fGarch`, `corrplot`, tidyverse.
