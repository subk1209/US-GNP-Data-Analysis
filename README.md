Financial Time Series analysis on US GNP dataset

Performed ACF and PACF analysis on quarterly US GNP data from 1948 to 2023, investigating presence of serial correlation and then employed EACF to determine the appropriate order of ARMA model, subsequently implementing an AR Model
Refined the AR model to get statistically significant lagged coefficients of the model improving AIC values and then tested for the randomness of residuals of the model using Ljung‐Box test and other factors like checking for ACF of the residuals
Examined the residual volatility stemming from the AR model and applied an ARCH (1) model (using fGarch package in R)
Implemented the finalized AR model with an estimated volatility of 0.86 to project the US GNP value for the 2nd Quarter
