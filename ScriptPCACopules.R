library('ggplot2')
library('forecast')
library('tseries')
library('FactoMineR')
# Copula package
library('copula')
# Fancy 3D plain scatterplots
library('scatterplot3d')
# Useful package to set ggplot plots one next to the other
library('grid')
library('MASS')
library('hexbin')
library("RColorBrewer")
library('VineCopula')
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
#Chargement des données
elec <- ts(scan('Conso3H2017.txt'),frequency=8, start=1)
tempPpi <- ts(scan('TempPERPIGNAN2017.txt'),frequency=8, start=1)
tempTls <- ts(scan('TempTOULOUSE-BLAGNAC2017.txt'),frequency=8, start=1)
tempMtp <- ts(scan('TempMONTPELLIER2017.txt'),frequency=8, start=1)
tempTbs <- ts(scan('TempTARBES-OSSUN2017.txt'),frequency=8, start=1)
tempMil <- ts(scan('TempMILLAU2017.txt'),frequency=8, start=1)

plot(tempTls)
#on enlève tendance et saisonnalité
delec <- stl(elec, s.window = "periodic")$time.series[,"remainder"]
dtempPpi <- stl(tempPpi, s.window = "periodic")$time.series[,"remainder"]
dtempTls <- stl(tempTls, s.window = "periodic")$time.series[,"remainder"]
dtempMtp <- stl(tempMtp, s.window = "periodic")$time.series[,"remainder"]
dtempTbs <- stl(tempTbs, s.window = "periodic")$time.series[,"remainder"]
dtempMil <- stl(tempMil, s.window = "periodic")$time.series[,"remainder"]

#On rabote à 2800 observations sans plus regarder le temps
delec <- as.numeric(delec[1:2800])
dtempMil <- as.numeric(dtempMil[1:2800])
dtempMtp <- as.numeric(dtempMtp[1:2800])
dtempPpi <- as.numeric(dtempPpi[1:2800])
dtempTbs <- as.numeric(dtempTbs[1:2800])
dtempTls <- as.numeric(dtempTls[1:2800])
plot(ts(delec, frequency=8, start=1), main = "Résidu Consommation Electricité")
#On crée notre dataframe pour la PCA
data <- data.frame("Millau" = dtempMil, "Montpellier" = dtempMtp, "Perpignan" = dtempPpi, 'Toulouse' = dtempTls, 'Tarbes' = dtempTbs)
str(data)
#On fait la PCA et on garde la première coordonnée pour nos plots de copules
pca <- PCA(data, scale.unit=FALSE, ncp=5, graph=T)
pca$eig
as.numeric(pca$var$coord[,1])
temp <- as.vector(as.matrix(data)%*%as.numeric(pca$var$cor[,1]))
plot(ts(temp, frequency=8, start=1), main = "Résidu Température Globale")
#Début de l'analyse des copules
scatterplot3d(temp,delec, color="red", main="Samples in 2D", xlab = "Temperature-modif", ylab="Conso-modif",pch=".")
df <- data.frame(temp,delec)
plot(temp)
h <- hexbin(df)
plot(h, colramp=rf)
hist(temp, breaks = 20, col = "blue", density = 20, freq = FALSE, main='Histogramme Temperature')
hist(rnorm( 2800, mean = ftemp$estimate[1], sd = ftemp$estimate[2]), breaks = 20,col = "red", add = T, density = 20, angle = -45, freq = FALSE)


#Estimation des marginales
felec <- fitdistr(delec,"normal")
ftemp <- fitdistr(temp,"normal")

hist(delec, breaks = 20, col = "green", density = 20, freq = FALSE, main='Histogramme Consommation')
hist(rnorm( 2800, mean = felec$estimate[1], sd = felec$estimate[2]), breaks = 20,col = "red", add = T, density = 20, angle = -45, freq = FALSE)

#Etude des taux de kendall et de spearman
cor(df)
cor(df, method = 'kendall')
cor(df, method = 'spearman')
m <- as.matrix(df)
mtilde <- as.matrix(data.frame(1-pobs(m)[,1],pobs(m)[,2]))

gf <- gofCopula(normalCopula(dim = 2), mtilde, estim.method = "itau", N = 50)
gf2 <- gofCopula(claytonCopula(dim = 2), mtilde, estim.method = "itau", N = 50)
gf3 <- gofCopula(gumbelCopula(dim = 2), mtilde, estim.method = "itau", N = 50)




cop_model <- claytonCopula(dim = 2)

fit <- fitCopula(cop_model, pobs(m), method = 'itau')
coef(fit)

#Simulation
# Build the bivariate distribution
my_dist <- mvdc(fit@copula, margins = c("norm","norm"), paramMargins = list(list(mean = ftemp$estimate[1], sd = ftemp$estimate[2]), list(mean = felec$estimate[1], sd = felec$estimate[2])))

# Generate random sample observations from the multivariate distribution
v <- rMvdc(2800, my_dist)
h <- hexbin(v)
plot(h, colramp=rf)



plot(v[,1], v[,2], main = 'Test', col = "blue")
points(m[,1], m[,2], col = 'red')
legend('bottomright', c('Observe', 'Simule'), col = c('blue', 'red'), pch=21)

x_mean <- mean(mydata$x)
x_var <- var(mydata$x)
x_rate <- x_mean / x_var
x_shape <- ( (x_mean)^2 ) / x_var
adf.test(delec, alternative = "stationary")
Acf(delec, main='Autocorrélation du résidu')
Pacf(delec, main='Autocorrélation partielle du résidu')
