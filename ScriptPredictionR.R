library('ggplot2')
library('forecast')
library('tseries')

test <- ts(scan('TestConsommationElectriqueHeureJanv2018.txt'),frequency=24, start=c(1,1)) 

#décomposition stl
decomp<-stl(test, s.window = "periodic")
residu <-decomp$time.series[,"remainder"]
trend <-decomp$time.series[,"trend"]
seasonal <-decomp$time.series[,"seasonal"]
plot(decomp)
#test de stationnarité du résidu
adf.test(residu, alternative = "stationary")
#Fonctions d'autocorrélation et d'autocorrélation partielle
plot(residu)
Acf(residu, main='Autocorrélation du résidu')
Pacf(residu, main='Autocorrélation partielle du résidu')
#On différencie pour voir si c'est mieux
diff1 <- diff(residu, differences = 1)
plot(diff1)
adf.test(diff1, alternative = "stationary")
Acf(diff1, main='Autocorrélation du résidu différencié une fois')
Pacf(diff1, main='Autocorrélation partielle du résidu différencié une fois')
diff2 <- diff(diff1, differences = 1)
plot(diff2)
adf.test(diff2, alternative = "stationary")
Acf(diff2, main='Autocorrélation du résidu différencié une fois')
Pacf(diff2, main='Autocorrélation partielle du résidu différencié deux fois')
#On garde la difference du residu et on essaie de reprérer la saison
decomp2 <- stl(diff1, s.window = "periodic")
residu2 <- seasadj(decomp2)
plot(decomp2)
plot(residu2)
#Une saisonnalite de l'ordre de 6H semble apparaître
#On va tester plusieurs SARIMA
#Les autocorr partielles semblent au plus d'ordre 3
#Les autocorr semblent au plus d'ordre 2
#Les saisonnalites aleatoires semblent d'ordre 6 non diff
#6 modeles à tester
fit<- arima(residu, order = c(3, 1, 2), seasonal = c(1, 0, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(3,1,2)(1,0,1) Model Residuals')
fit<- arima(residu, order = c(2, 1, 2), seasonal = c(1, 0, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(2,1,2)(1,0,1) Model Residuals')
fit<- arima(residu, order = c(2, 1, 1), seasonal = c(1, 0, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(2,1,1)(1,0,1) Model Residuals')
fit<- arima(residu, order = c(1, 1, 1), seasonal = c(1, 0, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1)(1,0,1) Model Residuals')
fit<- arima(residu, order = c(1, 1, 2), seasonal = c(1, 0, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(1,1,2)(1,0,1)Model Residuals')
fit<- arima(residu, order = c(2, 1, 2), seasonal = c(1, 1, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(2,1,2) (1,1,1) Model Residuals')
fit<- arima(residu, order = c(2, 1, 2), seasonal = c(2, 0, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(2,1,2)(2,0,1) Model Residuals')
fit<- arima(residu, order = c(2, 1, 2), seasonal = c(1, 0, 2), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(2,1,2)(1,0,2) Model Residuals')
fit<- arima(residu, order = c(2, 1, 2), seasonal = c(2, 0, 2), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(2,1,2)(2,0,2) Model Residuals')
fit<- arima(residu, order = c(2, 1, 2), seasonal = c(2, 1, 1), method = "CSS")
tsdisplay(residuals(fit), lag.max=45, main='(1,1,)(2,1,1) Model Residuals')
#On retient finalement 202 avec 101 ou 201
fit1<- arima(residu, order = c(2, 1, 2), seasonal = c(2, 0, 1), method = "CSS")
tsdisplay(residuals(fit1), lag.max=45, main='(2,1,2)(2,0,1) Model Residuals')

#Simulation pour dernier jour
fit_no_holdout = arima(ts(residu[-c(721:744)]), order=c(2,1,2), seasonal = c(2, 0, 1), method = "CSS")
data <- vector('numeric')
for (i in seq(1,10000))
  data <- c(data, sum(trend[-c(721:744)] + seasonal[-c(721:744)] + as.numeric(simulate(fit1,nsim=24))))
cat(data,file="SimulationConso1Fev.txt",sep="\n")
  
plot(data)
plot(foo)
arr <- as.numeric(foo)
dim(arr)
sum(arr)
conso_simulee = 
Box.test(residuals(fit1))
#On conserve 202 avec 101
hold <- window(ts(residu), start=721)
fit_no_holdout = arima(ts(residu[-c(721:744)]), order=c(2,1,2), seasonal = c(2, 0, 1), method = "CSS")
fcast_no_holdout <- forecast(fit_no_holdout,h=24)
plot(fcast_no_holdout, main="Prévision à 24heures du résidu par SARIMA (212)(201)")
lines(ts(residu))
ts1 <- ts(fcast_no_holdout$x, frequency = 24)
ts2 <- ts(fcast_no_holdout$mean, frequency = 24, start = 721)
ts3 <- ts(c(ts1,ts2), start = 1, frequency = 24)
alpha <- ts3 + trend + seasonal
plot(ts(alpha[-c(1:720)]), col="red", main="Prévision à 24heures de la série par SARIMA (212)(201)")
lines(ts(test[-c(1:720)]))

plot(predict(fit1,n.ahead = 216,se.fit = FALSE))
plot(forecast(fit1,h=24))
test<-ts(test[1:504],start = 1, frequency = 24)
comparaison <- seasadj(stl(test, s.window = "periodic"))
obj <- forecast(fit1,h=24)
alpha <-ts(c(obj$x,obj$mean), start = 1, frequency = 24)
ts.plot(alpha, a, gpars = list(col = c("black", "red")))
plot(comparaison)