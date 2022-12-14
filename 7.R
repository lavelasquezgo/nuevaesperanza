##
#  Pr?cticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  trav?s de la construcci?n de s?mbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Vel?squez Gonz?lez
#  Universidad Nacional de Colombia
#  2022
#
#  7. Comparaci?n arquitectura
#
#  Adaptado del c?digo de V?ctor Gonz?lez Fern?ndez (2019)
#
#  Cargar paquetes
#
library("plyr") # ddply()
library("plotrix") # battleship.plot()
library("RcmdrMisc") # numSummary()
library("ggplot2") # ggplot()
#
#  Cargar las funciones
#
source("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/7a")
#
#  Cargar los datos
#
area_casas <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/7b",
header = TRUE)
#
#  Periodo
#
#  Seriaci?n de la forma de las estructuras arquitect?nicas
#
forma_seriac <- data.frame(Periodo = c(rep("1", 22), rep("2", 34), rep("3", 31)), 
  Area = c(as.numeric(area_casas$Area[area_casas$Tardio_ == 1]),
  as.numeric(area_casas$Area[area_casas$Temprano_ == 1]),
  as.numeric(area_casas$Area[area_casas$Herrera_ == 1])),
  Forma = c(area_casas$Forma[area_casas$Tardio_ ==1],
  area_casas$Forma[area_casas$Temprano_ == 1],
  area_casas$Forma[area_casas$Herrera_ ==1]))
#
#  Tabulaci?n 
#
t <- table(forma_seriac$Periodo,forma_seriac$Forma)
t
#
#  Gr?fico de Ford
#
battleship.plot(t, col = "grey", maxxspan = 0.5, maxyspan = 0.2, 
yaxlab = c("Tard?o", "Temprano", "Herrera"))
#
#  Comparaci?n del ?rea de las estructuras circulares y rectangulares
#
#  Crear dataframe
#
circ_rect <- data.frame(Forma = c(area_casas$Forma[area_casas$Forma== "Circular"], 
area_casas$Forma[area_casas$Forma== "Rectangular"]),
?rea = c(area_casas$Area[area_casas$Forma== "Circular"],
area_casas$Area[area_casas$Forma== "Rectangular"]))
#
#  Hacer una prueba t de Student
#
t.test(circ_rect$?rea~circ_rect$Forma, var=TRUE)
# t = -3.2553, df = 44, p-value = 0.002182
#
#  Comparaci?n del ?rea de las estructuras circulares y rectangulares en los 
#  tres periodos
#
#  Crear dataframe
#
area_periodo <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/7c",
header = TRUE)
#
#  Tabular informaci?n del ?rea de las estructuras circulares seg?n el periodo
# 
circular <- subset(area_periodo, subset = area_periodo$Forma == "Circular")
#
#  Obtener medidas de tendencia central y de dispersi?n del ?rea de las
#  estructuras circulares para cada periodo
#
#  Herrera
#
numSummary(circular$Area[circular$Periodo == 1])
#
#  Muisca Temprano
#
numSummary(circular$Area[circular$Periodo == 2])
#
#  Muisca Tard?o
#
numSummary(circular$Area[circular$Periodo == 3])
#
#  ANOVA del ?rea de las estructuras circulares en los tres periodos
#
with(subset(area_periodo, subset = area_periodo$Forma == "Circular"), boxplot(Area~Periodo))
model <- with(subset(area_periodo, subset = area_periodo$Forma == "Circular"), aov(Area~Periodo))
model
summary(model)
#
#  Tabular informaci?n del ?rea de las estructuras rectangulares seg?n el periodo
#
rectangular <- subset(area_periodo, subset = area_periodo$Forma == "Rectangular")
#
#  Obtener medidas de tendencia central y de dispersi?n del ?rea de las estructuras
#  rectangulares para cada periodo
#
#  Herrera
#
numSummary(rectangular$Area[rectangular$Periodo == 1])
#
#  Muisca Temprano
#
numSummary(rectangular$Area[rectangular$Periodo == 2])
# 
#  Muisca Tard?o
#
numSummary(rectangular$Area[rectangular$Periodo == 3])
#
#  ANOVA del ?rea de las estructuras rectangulares en los tres periodos
#
with(subset(area_periodo, subset = area_periodo$Forma == "Rectangular"), boxplot(Area~Periodo))
model <- with(subset(area_periodo, subset = area_periodo$Forma == "Rectangular"), aov(Area~Periodo))
model
summary(model)
#
#  ?rea
#
#  Histograma con media y desviaci?n est?ndar
#
texto <- paste("Media =", round(mean(area_casas$Area),3), "\n sd = ",
round(sd(area_casas$Area), 3))
hist(area_casas$Area, ylab="Frecuencia", xlab="?rea",
main = paste("Histograma de las ?reas \n  de las estructuras arquitect?nicas")) 
text(x = 200, y = 15, cex = 0.8,labels = texto)
#
#  Comparaci?n de los promedios y rangos de error del ?rea de las estructuras seg?n
#  los grupos
#
balasB(area_casas$Grupo, area_casas$Area, xlab= "Grupo", ylab = "Area", an = 1.2)
text(x=1.2,y=180,cex=.7,label="Confianza:  \n  \u2590\u2588\ 80%  \u2588 95%  \u2590 99%", col=1)
#
#  Forma
#
#  Comparacion de las proporciones y los rangos de error de estructuras 
#  arquitect?nicas de diferente forma seg?n los grupos
#
#  Proporciones de estructuras circulares
#
balapropRE(area_casas$Grupo, area_casas$Forma, 1)
#
#  Proporciones de estructuras ovales
#
balapropRE(area_casas$Grupo, area_casas$Forma, 2)
#
#  Proporciones de estructuras rectangulares
#
balapropRE(area_casas$Grupo, area_casas$Forma, 3)
#
#  Reconstrucciones
#
#  Comparacion de las proporciones y los rangos de error de estructuras 
#  arquitect?nicas con al menos una reconstrucci?n seg?n los grupos
#
balapropRE(area_casas$Grupo, area_casas$Reconstruida, 2)
#
##