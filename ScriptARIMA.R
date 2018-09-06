library('ggplot2')
library('forecast')
library('tseries')

residu <- ts(scan('Residu Buys-Ballot Conso electrique hebdomadaire 2015-2017.txt'))

adf.test(residu, alternative = "stationary")

Acf(residu, main='')

Pacf(residu, main='')

fit<-auto.arima(residu, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=30, main='Résidus du modèle ARIMA(4,0,2)')

fcast <- forecast(fit, h=5)
plot(fcast)
ts(fcast$mean)
cat(fit$fitted, file="outfile.txt")

fit$fitted

plot(predict(fit1,n.ahead = 216,se.fit = FALSE))
plot(forecast(fit1,h=24))
test<-ts(test[1:504],start = 1, frequency = 24)
comparaison <- seasadj(stl(test, s.window = "periodic"))
obj <- forecast(fit1,h=24)
alpha <-ts(obj$x, start = 1, frequency = 24)
ts.plot(ts(forecast(fit1,h=24), start = 1, frequency = 24), comparaison, gpars = list(col = c("black", "red")))
plot(comparaison)
ts(c(residu,residusfcsts), start=start(residu), frequency=frequency(residu))