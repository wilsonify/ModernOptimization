# this solution assumes that file "tsf.R" has already been executed

library(pso) # load pso

# evaluation function of arma coefficients:
evalarma <- function(s)
{ a <- suppressWarnings(arima(sunspots, order = c(AR, 0, MA), fixed = s))
  R <- a$residuals[INIT:length(sunspots)]
  R <- maeres(R)
  if (is.nan(R)) R <- Inf # death penalty
  return(maeres(R))
}

AR <- 2; MA <- 1
maxit <- 100; LP <- 50
meants <- mean(sunspots); K <- 0.1 * meants
lower <- c(rep(-1, (AR + MA)), meants - K)
upper <- c(rep(1, (AR + MA)), meants + K)
C <- list(maxit = maxit, s = LP, trace = 10, REPORT = 10)
set.seed(12345) # set for replicability
PSO <- psoptim(rep(NA, length(lower)), fn = evalarma,
               lower = lower, upper = upper, control = C)
arima2 <- arima(sunspots, order = c(AR, 0, MA), fixed = PSO$par)
print(arima2)
cat("pso fit MAE=", PSO$value, "\n")

# one-step ahead predictions:
f3 <- rep(NA, forecasts)
for (h in 1:forecasts)
{ # execute arima with fixed coefficients but with more in-samples:
  arima1 <- arima(series[1:(LIN + h - 1)], order = arima2$arma[c(1, 3, 2)], fixed = arima2$coef)
  f3[h] <- forecast(arima1, h = 1)$mean[1]
}
e3 <- maeres(outsamples - f3)
text3 <- paste0("pso arima (MAE=", round(e3, digits = 1), ")")

# show quality of one-step ahead forecasts: 
ymin <- min(c(outsamples, f1, f3))
ymax <- max(c(outsamples, f1, f3))
par(mar = c(4.0, 4.0, 0.1, 0.1))
plot(outsamples, ylim = c(ymin, ymax), type = "b", pch = 1,
     xlab = "time (years after 1980)", ylab = "values", cex = 0.8)
lines(f1, lty = 2, type = "b", pch = 3, cex = 0.5)
lines(f3, lty = 3, type = "b", pch = 5, cex = 0.5)
legend("topright", c("sunspots", text1, text3), lty = 1:3, pch = c(1, 3, 5))
