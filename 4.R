##
#  Pr?cticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  trav?s de la construcci?n de s?mbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Vel?squez Gonz?lez
#  Universidad Nacional de Colombia
#  2022
#
#  4. Seriaci?n cer?mica por corte
#
#  Adaptado del c?digo de V?ctor Gonz?lez Fern?ndez (2019)
#
#  Cargar paquetes
#
library(RcmdrMisc) # rowPercents()
library(plotrix)   # battleship.plot()
#
#  Cargar datos
#
ceramicadiag <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/4a",
header=TRUE)
names(ceramicadiag)
ceramicadiag$Corte <- paste0("C_",ceramicadiag$Corte)
ceramicadiag$Tipos_Ceramicos <- factor(ceramicadiag$Tipos_Ceramicos)
#
#  Tabla de porcentajes
#
mtc_porcent <- list()
for (i in ceramicadiag$Corte) {
  mtc_porcent[i] <- list(rowPercents(table(data.frame(
    Nivel = ceramicadiag$Nivel[ceramicadiag$Corte == i], 
    Tipo = ceramicadiag$Tipos_Ceramicos[ceramicadiag$Corte == i]))))
}
#  Listado de los cortes
#
names(mtc_porcent)
#
#  Gr?fico de seriaci?n de los cortes de excavaci?n en los que se 
#  encuentran las estructuras arquitect?nicas
#
#  Funci?n para graficar
#
serie <- function(x){
if (length(x[,1])>1)
battleship.plot(x[1:(length(x[,1])),1:(length(x[1,])-2)], col="gray34",
 maxyspan=0.5, border="gray34", mar=c(0,1,7,5), cex.labels=.8)
}
#
#  Graficar seriaci?n del Corte 35 (Estructura 38)
#
serie(mtc_porcent$C_35)
#
#  Para graficar la seriaci?n de otro corte s?lo se debe cambiar el 
#  n?mero del corte. Por ejemplo, para el corte 38:
#
serie(mtc_porcent$C_38)
#
#  Graficos de todos los cortes con m?s de un nivel
# 
par(mfrow=c(4,2))
for (i in 1:length(mtc_porcent)){
 Sys.sleep(.2)
 serie(mtc_porcent[[i]])
 if (length(mtc_porcent[[i]][,1])>1)
 title(names(mtc_porcent)[i],adj=0)
 Sys.sleep(.2)
}
##
