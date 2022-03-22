
library(astsa)
library(tseries)
library(ggplot2)
library(forecast)

eth <- read.csv("ETHUSD.csv")

eth$Return <- (as.numeric(eth$Open)) - (as.numeric(eth$Close))
eth$Date <- as.Date(eth$Date)
eth_data <- subset(eth, Date < ("2021-06-06"))

ggplot(eth_data, aes(x=Date)) +  geom_line(aes( y = as.numeric(Close))) + xlab("Date") + ylab("Price($)")
ggplot(eth_data, aes(x=Date)) +  geom_line(aes( y = as.numeric(Return))) + xlab("Date")

eth_rt <- eth_data$Return

eth_arima <- auto.arima(eth_data$Return, allowmean = F)
ar_p <- arimaorder(eth_arima)[1] 
ma_q <- arimaorder(eth_arima)[3]  

ar_resid <- eth_arima$residuals 
par(mfrow=c(2,1))
acf((eth_arima$residuals), main= "ACF plot for ARMA(5,3) residuals")
acf((eth_arima$residuals)^2, main= "ACF plot for squared residuals")

ar_fit <- eth_arima$fitted

garch_fit <- garch(ar_resid, order=c(1,1))
garch1 <- garch(eth_rt, order = c(1,1))

ar_gar_pd <- predict(garch_fit)

uk <- predict(garch1)

plot(eth_rt, type = "l", xlab= "Time", ylab = "Returns", main = "ARMA-GARCH Predictions for ETH volatility")
lines(ar_gar_pd[,1]+ar_fit, col = "red", lty = "dashed")
lines(ar_gar_pd[,2]-ar_fit, col = "red", lty = "dashed")

plot(eth_rt, type = "l", xlab = "Time", ylab = "Returns", main = "GARCH predictions for ETH volatility")
lines(uk[,1], col = "red", lty = "dashed")
lines(uk[,2], col = "red", lty = "dashed")