##
#  Pr?cticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  trav?s de la construcci?n de s?mbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Vel?squez Gonz?lez
#  Universidad Nacional de Colombia
#  2022
#
#  10. Comparaci?n cer?mica
#
#  Adaptado del c?digo de V?ctor Gonz?lez Fern?ndez (2019)
#
#  Cargar paquetes
#
library("RcmdrMisc")
library("plyr")
#
#  Cargar las funciones
#
source("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/10a")
#
#  Cargar los datos
#
Forma_total <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/10b",
header = TRUE)
#
#  Comparaci?n de proporciones de formas cer?micas entre los grupos
#
round(prop.table(table(Forma_total$Forma)), 2)
#
par(mfrow=c(1,2))
with(Forma_total, balasP2(Grupo,Forma,1, ylim=c(0.0,0.02)))
with(Forma_total, balasP2(Grupo,Forma,3, ylim=c(0.0,0.04)))
par(mfrow=c(1,1))
#
par(mfrow=c(1,2))
with(Forma_total, balasP2(Grupo,Forma,2, ylim=c(0.4,0.7)))
with(Forma_total, balasP2(Grupo,Forma,4, ylim=c(0.3,0.6)))
par(mfrow=c(1,1))
#
#  Comparaci?n de proporciones de formas cer?micas del periodo Herrera entre los grupos
#
round(prop.table(table(Forma_total$Forma[Forma_total$Periodo == "Herrera"])),2)
#
par(mfrow=c(1,2))
with(Forma_Herrera, balasP2(Grupo,Forma,1, ylim=c(-0.002,0.02)))
with(Forma_Herrera, balasP2(Grupo,Forma,3, ylim=c(-0.002,0.04)))
par(mfrow=c(1,1))
#
par(mfrow=c(1,2))
with(Forma_Herrera, balasP2(Grupo,Forma,2, ylim=c(0.6,0.8)))
with(Forma_Herrera, balasP2(Grupo,Forma,4, ylim=c(0.2,0.4)))
par(mfrow=c(1,1))
#
#  Comparaci?n de proporciones de formas cer?micas del periodo Temprano entre los grupos
#
round(prop.table(table(Forma_total$Forma[Forma_total$Periodo == "Temprano"])),2)
#
par(mfrow=c(1,2))
with(Forma_Temprano, balasP2(Grupo,Forma,1, ylim=c(-0.001,0.03)))
with(Forma_Temprano, balasP2(Grupo,Forma,3, ylim=c(0.005,0.05)))
par(mfrow=c(1,1))
#
par(mfrow=c(1,2))
with(Forma_Temprano, balasP2(Grupo,Forma,2, ylim=c(0.3,0.7)))
with(Forma_Temprano, balasP2(Grupo,Forma,4, ylim=c(0.3,0.7)))
par(mfrow=c(1,1))
#
#  Comparaci?n de proporciones de formas cer?micas del periodo Tard?o entre los grupos
#
round(prop.table(table(Forma_total$Forma[Forma_total$Periodo == "Tardio"])),2)
#
par(mfrow=c(1,2))
with(Forma_Tardio, balasP2(Grupo,Forma,1, ylim=c(-0.001,0.05)))
with(Forma_Tardio, balasP2(Grupo,Forma,3, ylim=c(0.005,0.06)))
par(mfrow=c(1,1))
#
par(mfrow=c(1,2))
with(Forma_Tardio, balasP2(Grupo,Forma,2, ylim=c(0.25,0.75)))
with(Forma_Tardio, balasP2(Grupo,Forma,4, ylim=c(0.25,0.75)))
par(mfrow=c(1,1))
#
#  Comparaci?n de los di?metros de las ollas
#
#  Cargar datos
#
diametro_ollas <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/10c",
header = TRUE)
diametro_ollas
#
#  Resumen grupos
#
medias <- data.frame(Media = c(mean(diametro_ollas$Diametro[diametro_ollas$Grupo == 1]),
mean(diametro_ollas$Diametro[diametro_ollas$Grupo == 2]),
mean(diametro_ollas$Diametro[diametro_ollas$Grupo == 3]),
mean(diametro_ollas$Diametro[diametro_ollas$Grupo == 4])),
Desviacion = c(sd(diametro_ollas$Diametro[diametro_ollas$Grupo == 1]),
sd(diametro_ollas$Diametro[diametro_ollas$Grupo == 2]),
sd(diametro_ollas$Diametro[diametro_ollas$Grupo == 3]),
sd(diametro_ollas$Diametro[diametro_ollas$Grupo == 4]))
)
medias
#
#  Comparar los di?metros por periodo
#
#  Herrera
#
balasB(diametro_ollas$Grupo[diametro_ollas$Periodo == "Herrera"], diametro_ollas$Diametro[diametro_ollas$Periodo == "Herrera"], ylab = "Di?metro", xlab = "Grupo")+
  text(x=1.3,y=23.5,cex=.7,label="Confianza:  \n  \u2590\u2588\ 80%  \u2588 95%  \u2590 99%", col=1)
#
#  Temprano
#
balasB(diametro_ollas$Grupo[diametro_ollas$Periodo == "Temprano"], diametro_ollas$Diametro[diametro_ollas$Periodo == "Temprano"], ylab = "Di?metro", xlab = "Grupo")+
  text(x=1.3,y=18.3,cex=.7,label="Confianza:  \n  \u2590\u2588\ 80%  \u2588 95%  \u2590 99%", col=1)
#
#  Tard?o
#
balasB(diametro_ollas$Grupo[diametro_ollas$Periodo == "Tardio"], diametro_ollas$Diametro[diametro_ollas$Periodo == "Tardio"], ylab = "Di?metro", xlab = "Grupo")+
  text(x=1.3,y=18.3,cex=.7,label="Confianza:  \n  \u2590\u2588\ 80%  \u2588 95%  \u2590 99%", col=1)
#
numSummary(diametro_ollas$Diametro[diametro_ollas$Periodo == "Herrera"])
numSummary(diametro_ollas$Diametro[diametro_ollas$Periodo == "Temprano"])
numSummary(diametro_ollas$Diametro[diametro_ollas$Periodo == "Tardio"])
#
##