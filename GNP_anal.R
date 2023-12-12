library(tidyverse)
library(fGarch)
library(TSA)


df <- read.csv("D:/Projects/Time Series Project/data/GNP data.csv")
View(df)

glimpse(df)
View(df)

colnames(df) <- c('Date', "Change")

df$Date <- as.Date(df$Date, format = '%Y-%m-%d')

max(df$Date) - min(df$Date)

# Plots:
df %>% ggplot(aes(x = Date, y = Change)) + 
  geom_line(colour = 'red') + theme_light() +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "3 years") + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  geom_hline(yintercept = 0, colour = 'blue', lty = 2) 


# plot(diff(df$Change), type = 'l')


# ACF $ PACF:
acf(df$Change, lag.max = 100)
pacf(df$Change) # sinusoidal decay (=> stationary)
pacf(diff(df$Change), lag.max = 30)

# eacf(df$Change)

# Models:
a1 <- ar(df$Change, method = 'mle')
names(a1)



a1$order

a1$aic %>% plot(type = 'o', pch = 19)
abline(h = 0)  # AIC


# AR:
ar_model <- arima(df$Change, order = c(13,0,0),
                  fixed = c(rep(NA,5), 0, 0, 0, 
                            rep(NA,2), 0, 0, NA, NA))
ar_model

arima(df$Change, order = c(13,0,0))


arima(df$Change, order = c(10,1,0))
arima(diff(df$Change), order = c(10,0,0))
plot(diff(df$Change), type = 'l')
plot(df$Change, type = 'l')

arima(df$Change, order = c(13,0,0)) -> a1

TSA::eacf(df$Change)





p.func <- function(highest_lag, res){
  pval <- 0
  for(i in 1:highest_lag) (pval[i] = 
                             Box.test(res, lag = i,
                                      type = "Ljung-Box")$p.value)
  
  data.frame('Lags' = 1:highest_lag, 'Pvalue' = pval) %>% 
    ggplot(aes(x = Lags, y = Pvalue)) + 
    geom_point(size = 2) +
    geom_hline(yintercept = 0.05,
               linetype = 2, colour = 'red') +
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10) +
    annotate("text", x = 5, y = 0.07,
             label = "Threshold = 0.05",
             size = 4, colour = 'blue') +
    theme_bw()
}

p.func(50, a1$residuals)


arima(df$Change, order = c(13,0,0),
      fixed = c(NA,0,0,NA,NA,0,0,NA,NA,0,0,NA,NA,NA)) -> a2
a2

acf(a2$residuals, main = '')
acf(abs(a2$residuals), main = '')


# fitting arch 4:
arch4 <- garchFit(~1 + garch(4,0), data = a2$residuals, trace = F,
                  include.mean = F)
summary(arch4)

# fitting arch 1:
arch2 <- garchFit(~1 + garch(1,0), data = a2$residuals, trace = F,
                  include.mean = F)
summary(arch2)


sigma_301 <- volatility(arch2)[301]
sigma_302 <- sqrt()
a2$residuals[301]/rnorm(1)


# forecast:
sigma_sq_302 <- 0.72497 + 0.65367*(a2$residuals[301])^2
sqrt(sigma_sq_302)





# eacf(df$Change)
#===================================================================
# ARCH GARCH BAAL BICHI
n <- nrow(df); n
yt <- df$Change[2:n]/df$Change[1:(n-1)]
plot(yt, type = 'l')

par(mfrow = c(1,2))
acf(yt)
acf(yt^2)

Box.test(at, lag = 4, type = 'Ljung') # accept at 1%
Box.test(yt^2, lag = 4, type = 'Ljung') # reject!
p.func(30, yt)



pacf(yt)
ar_model <- arima(yt, order = c(4,0,0), fixed = c(rep(0,3),NA,NA))
at <- residuals(ar_model)


plot(at, type = 'l')
lines(yt, type = 'l', col = 'red')


p.func(30, at)
p.func(30, at^2)
acf(at)
acf(at^2)

# p-value of F-stat of regression
f(at, 4)


arch4 <- garchFit(~1 + garch(4,0), data = at, trace = F)
summary(arch4)







  # Function to calculate p-value from multiple linear regression
f <- function(x, m){
  
  n <- length(x); n
  M <- as.data.frame(matrix(ncol = m+1, nrow = n-m,
                            dimnames = list(1:(n-m),
                                            c('y',paste0('x',1:m)))))
  
  for(i in (m+1):1) (M[,i] <- (x[i:(n-m-1+i)])^2)
  
  cname <- names(M); cname
  
  reformulate(cname[2:(m+1)], response = 'y') -> f
  f %>% lm(data = M) %>% summary() -> s
  pf(s$fstatistic, m, n-2*m-1, lower.tail = F)[[1]] %>% 
    return()
}

# p-value of F-statistic:
f(diff(df$Change), 3)

pacf(yt^2)
# ARCH(8) is appropriate.

t.test(yt) # mean is significantly equal to 0.

# Fitting of ARCH(8):
arch8 <- garchFit(~1 + garch(8,0), data = yt, trace = F,
                  include.mean = F)
summary(arch8)

arch4 <- garchFit(~1 + garch(4,0), data = yt, trace = F,
                  include.mean = F, cond.dist = 'std')
summary(arch4)


res1 <- residuals(arch4, standardize = T)
plot(res1, type= 'l')


# Q-Q plot for checking the normality of the residuals
qqnorm(res1, pch = 20, col = 'red')
abline(a = 0, b = 1, col = 'blue', lty = 2, lwd = 2)
#===================================================================
# Fitting of GARCH:
garch11 <- garchFit(~1 + garch(1,1), data = yt, trace = F,
                    cond.dist = 'sstd', include.mean = F)
summary(garch11)

{
  garch12 <- garchFit(~1 + garch(1,2), data = yt, trace = F)
  summary(garch12)
  
  garch21 <- garchFit(~1 + garch(2,1), data = yt, trace = F)
  summary(garch21)
}

res2 <- residuals(garch11, standardize = T)
plot(res2, type = 'l')

# Q-Q plot for checking the normality of the residuals
qqnorm(res2, pch = 20, col = 'red')
abline(a = 0, b = 1, col = 'blue', lty = 2, lwd = 2)


{
ks.test(res1, 'dnorm')
ks.test(res2, 'dnorm')
}



