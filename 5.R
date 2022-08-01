##
#  Prácticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  través de la construcción de símbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Velásquez González
#  Universidad Nacional de Colombia
#  2022
#
#  5. Vecino más cercano
#
#  Adaptado del código de Víctor González Fernández (2019)
#
#  Cargar paquetes
#
library(spatstat)  #  clarkevans.test()
#
#  Vecino más cercano de estructuras arquitectónicas
#
#  Cargar datos de estructuras arquitectónicas
#
casas_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/5a",
header = TRUE)
#
#  Crear dataframe
#
casas_NES_NN <- data.frame(X = casas_NES$X, Y = casas_NES$Y)
#
#  Encontrar n
#
n <- NROW(casas_NES_NN)
n
#
#  Encontrar la ventana rectangular más pequeña
#
WIN1_casas <- ripras(casas_NES_NN, shape="rectangle", f = 1)
#
#  Graficar
#
plot(casas_NES_NN$X, casas_NES_NN$Y, pch = 21, col = "red", asp = 1) 
plot(WIN1_casas, lty = 5, add = T)
#
#  Convertir al formato ppp:
#
casasPPP <- as.ppp(casas_NES_NN,WIN1_casas)
plot(casasPPP,pch=16,cex=0.5)
#
#  Análisis de vecino más cercano de Clark y Evans: 
#
clarkevans.test(casasPPP)
#
#  Hay una confianza del 99.9% de que el patrón agrupado no se deba a un error del muestreo
#
#  Calcular el área de la ventana WIN1_casas
#
AWIN1_casas <- area.owin(WIN1_casas)
AWIN1_casas
#
#  Calcular distancias al vecino más cercano
#
NND_casas <- nndist(casas_NES_NN)
hist(NND_casas, col = 8)
#
#  Media observada de las distancias	
#
MNND_casas <- mean(NND_casas)
MNND_casas
#
#  Rango de error media observada
#
t.test(NND_casas)
mean(NND_casas)-t.test(NND_casas)$conf.int[1]
#
#  Estimamos que la media observada es de 12.04382 ± 1.37 m al 95% de confianza.
#
#  Media esperada de las distancias
#
EXPNND_casas <- 0.5/sqrt(n/AWIN1_casas)
EXPNND_casas
boxplot(NND_casas, col = 8)
abline(EXPNND_casas, 0, col = 2, lty = 5)
#
#  Rango de error de la media esperada
#
muestra_e <- rnorm(n= 48, mean = 16.88627, sd = sd(NND_casas))
t.test(muestra_e)
mean(muestra_e)-t.test(muestra_e)$conf.int[1]
#
#  Estimamos que la media de la población esperada es de 16.88627 ± 1.3 m al 95% de confianza.
#
#  Calcular "R"
#
MNND_casas / EXPNND_casas
#   Valor R = 0.7132317
#
#  Significancia (Pinder, Shimada, y Gregory 1979)
#
SE <- (0.26136 * sqrt(AWIN1_casas))/n
t  <- (EXPNND_casas - MNND_casas)/SE
dt(t, df = n-1)
1-dt(t, df = n-1)
#
#  Valor p =  0.0006382677
#
#  Hay una confianza del 99.9% de que el patrón agrupado no se deba a un error del muestreo
#
#  Vecino más cercano de tumbas
#
#  Cargar datos de las tumbas
#
tumbas_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/5b",
header = TRUE)
#
#  Crear dataframe
#
tumbas_NES_NN <- data.frame(X = tumbas_NES$X, Y = tumbas_NES$Y)
#
#  Encontrar n
#
n <- NROW(tumbas_NES_NN)
n
#
#  Encontrar la ventana rectangular más pequeña
#
WIN1_tumbas <- ripras(tumbas_NES_NN, shape="rectangle", f = 1)
#
#  Graficar
#
plot(tumbas_NES_NN$X, tumbas_NES_NN$Y, pch = 21, col = "red", asp = 1) 
plot(WIN1_tumbas, lty = 5, add = T)
#
#  Convertir al formato ppp:
#
tumbasPPP <- as.ppp(tumbas_NES_NN,WIN1_tumbas)
plot(tumbasPPP, pch=16, cex=0.5)
#
#  Análisis de vecino más cercano de Clark y Evans: 
#
clarkevans.test(tumbasPPP)
#
#  Hay una confianza de más del 99.9% de que el patrón agrupado no se deba a un error del muestreo
#
#  Calcular el área de la ventana WIN1_tumbas
#
AWIN1_tumbas <- area.owin(WIN1_tumbas)
AWIN1_tumbas
#
#  Calcular distancias al vecino más cercano
#
NND_tumbas <- nndist(tumbas_NES_NN)
hist(NND_tumbas, col = 8)
#
#  Media observada de las distancias	
#
MNND_tumbas <- mean(NND_tumbas)
MNND_tumbas 
#
#  Rango de error de la media observada
#
t.test(NND_tumbas)
mean(NND_tumbas)-t.test(NND_tumbas)$conf.int[1]
#
#  Estimamos que la media observada es de 2.161742 ± 0.11 m al 95% de confianza.
#
#  Media esperada de las distancias
#
EXPNND_tumbas  <- 0.5/sqrt(n/AWIN1_tumbas)
EXPNND_tumbas
boxplot(NND_tumbas, col = 8)
abline(EXPNND_tumbas, 0, col = 2, lty = 5)
#
#  Rango de error de la media esperada
#
muestra_e <- rnorm(n=1352, mean=3.64793, sd = sd(NND_tumbas))
t.test(muestra_e)
mean(muestra_e)-t.test(muestra_e)$conf.int[1]
#
#  Estimamos que la media esperada es de 3.64793 ± 0.11 m al 95% de confianza.
#
#  Calcular "R"
#
MNND_tumbas / EXPNND_tumbas
#   Valor R = 0.5925941
#
#  Significancia (Pinder, Shimada, y Gregory 1979)
#
SE <- (0.26136 * sqrt(AWIN1_tumbas))/n
t  <- (EXPNND_tumbas - MNND_tumbas)/SE
dt(t, df = n-1)
1-dt(t, df = n-1)
#
#  Valor p = 1.473279e-140
#
#  Hay una confianza de más del 99.99% de que el patrón agrupado no se deba a un error del muestreo
#
#  Vecino más cercano de depositos
#
#  Cargar datos de depósitos
#
depositos_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/5c",
header = TRUE)
#
#  Crear dataframe
#
depositos_NES_NN <- data.frame(X = depositos_NES$X, Y = depositos_NES$Y)
#
#  Encontrar n
#
n <- NROW(depositos_NES_NN)
n
#
#  Encontrar la ventana rectangular más pequeña
#
WIN1_depositos <- ripras(depositos_NES_NN, shape="rectangle", f = 1)
#
#  Graficar
#
plot(depositos_NES_NN$X, depositos_NES_NN$Y, pch = 21, col = "red", asp = 1) 
plot(WIN1_depositos, lty = 5, add = T)
#
#  Convertir al formato ppp:
#
depPPP <- as.ppp(depositos_NES_NN,WIN1_depositos)
plot(depPPP,pch=16,cex=0.5)
#
#  Análisis de vecino más cercano de Clark y Evans: 
#
clarkevans.test(depPPP)
#
#  Hay una confianza de más del 99.9% de que el patrón agrupado no se deba a un error del muestreo
#
#  Calcular el área de la ventana WIN1_depositos
#
AWIN1_depositos <- area.owin(WIN1_depositos)
AWIN1_depositos
#
#  Calcular distancias al vecino más cercano
#
NND_depositos <- nndist(depositos_NES_NN)
hist(NND_depositos, col = 8)
#
#  Media observada de las distancias	
#
MNND_depositos <- mean(NND_depositos)
MNND_depositos 
#
#  Rango de error media observada
#
t.test(NND_depositos)
mean(NND_depositos)-t.test(NND_depositos)$conf.int[1]
#
#  Estimamos que la media de la población es de 2.371326 ± 0.07 m al 95% de confianza.
#
#  Media esperada de las distancias
#
EXPNND_depositos  <- 0.5/sqrt(n/AWIN1_depositos)
EXPNND_depositos
boxplot(NND_depositos, col = 8)
abline(EXPNND_depositos, 0, col = 2, lty = 5)
#
#  Rango de error de la media esperada
#
muestra_e <- rnorm(n= 1720, mean = 3.355561, sd = sd(NND_depositos))
muestra_e
#
t.test(muestra_e)
mean(muestra_e)-t.test(muestra_e)$conf.int[1]
#
#  Estimamos que la media de la población es de 3.355561 ± 0.07 m al 95% de confianza.
#
#  Calcular "R"
#
MNND_depositos / EXPNND_depositos
#   Valor R = 0.7066856
#
#  Significancia (Pinder, Shimada, y Gregory 1979)
#
SE <- (0.26136 * sqrt(AWIN1_depositos))/n
t  <- (EXPNND_depositos - MNND_depositos)/SE
dt(t, df = n-1)
1-dt(t, df = n-1)
#
#  Valor p = 2.042466e-103
#
#  Hay una confianza de más del 99.99% de que el patrón agrupado no se deba a un error del muestreo
#
##
