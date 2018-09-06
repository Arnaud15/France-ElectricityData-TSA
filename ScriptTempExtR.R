library('fExtremes')
library('ggplot2')
library('forecast')

data <- ts(scan('SerieTemperatures20152017.txt'),frequency=16, start=c(1,1)) 

plot(stl(data, s.window = "periodic"))

#décomposition stl
decomp<-stl(data, s.window = "periodic")
residu <-decomp$time.series[,"remainder"]
trend <-decomp$time.series[,"trend"]
seasonal <-decomp$time.series[,"seasonal"]

adf.test(residu, alternative = "stationary")

Acf(residu, main='Autocorrélation du résidu')
Pacf(residu, main='Autocorrélation partielle du résidu')

diff1 <- diff(residu, differences = 1)
plot(diff1)
adf.test(diff1, alternative = "stationary")
Acf(diff1, main='Autocorrélation du résidu différencié une fois')
diff2 <- diff(diff1, differences = 1)
plot(diff2)
adf.test(diff2, alternative = "stationary")
Acf(diff2, main='Autocorrélation du résidu différencié deux fois')
Pacf(diff2, main='Autocorrélation partielle du résidu différencié deux fois')