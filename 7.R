##
#  Prácticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  través de la construcción de símbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Velásquez González
#  Universidad Nacional de Colombia
#  2022
#
#  7. Comparación arquitectura
#
#  Adaptado del código de Víctor González Fernández (2019)
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
#  Seriación de la forma de las estructuras arquitectónicas
#
forma_seriac <- data.frame(Periodo = c(rep("1", 22), rep("2", 34), rep("3", 31)), 
  Area = c(as.numeric(area_casas$Area[area_casas$Tardio_ == 1]),
  as.numeric(area_casas$Area[area_casas$Temprano_ == 1]),
  as.numeric(area_casas$Area[area_casas$Herrera_ == 1])),
  Forma = c(area_casas$Forma[area_casas$Tardio_ ==1],
  area_casas$Forma[area_casas$Temprano_ == 1],
  area_casas$Forma[area_casas$Herrera_ ==1]))
#
#  Tabulación 
#
t <- table(forma_seriac$Periodo,forma_seriac$Forma)
t
#
#  Gráfico de Ford
#
battleship.plot(t, col = "grey", maxxspan = 0.5, maxyspan = 0.2, 
yaxlab = c("Tardío", "Temprano", "Herrera"))
#
#  Comparación del área de las estructuras circulares y rectangulares
#
#  Crear dataframe
#
circ_rect <- data.frame(Forma = c(area_casas$Forma[area_casas$Forma== "Circular"], 
area_casas$Forma[area_casas$Forma== "Rectangular"]),
Área = c(area_casas$Area[area_casas$Forma== "Circular"],
area_casas$Area[area_casas$Forma== "Rectangular"]))
#
#  Hacer una prueba t de Student
#
t.test(circ_rect$Área~circ_rect$Forma, var=TRUE)
# t = -3.2553, df = 44, p-value = 0.002182
#
#  Comparación del área de las estructuras circulares y rectangulares en los 
#  tres periodos
#
#  Crear dataframe
#
area_periodo <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/7c",
header = TRUE)
#
#  Tabular información del área de las estructuras circulares según el periodo
# 
circular <- subset(area_periodo, subset = area_periodo$Forma == "Circular")
#
#  Obtener medidas de tendencia central y de dispersión del área de las
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
#  Muisca Tardío
#
numSummary(circular$Area[circular$Periodo == 3])
#
#  ANOVA del área de las estructuras circulares en los tres periodos
#
with(subset(area_periodo, subset = area_periodo$Forma == "Circular"), boxplot(Area~Periodo))
model <- with(subset(area_periodo, subset = area_periodo$Forma == "Circular"), aov(Area~Periodo))
model
summary(model)
#
#  Tabular información del área de las estructuras rectangulares según el periodo
#
rectangular <- subset(area_periodo, subset = area_periodo$Forma == "Rectangular")
#
#  Obtener medidas de tendencia central y de dispersión del área de las estructuras
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
#  Muisca Tardío
#
numSummary(rectangular$Area[rectangular$Periodo == 3])
#
#  ANOVA del área de las estructuras rectangulares en los tres periodos
#
with(subset(area_periodo, subset = area_periodo$Forma == "Rectangular"), boxplot(Area~Periodo))
model <- with(subset(area_periodo, subset = area_periodo$Forma == "Rectangular"), aov(Area~Periodo))
model
summary(model)
#
#  Área
#
#  Histograma con media y desviación estándar
#
texto <- paste("Media =", round(mean(area_casas$Area),3), "\n sd = ",
round(sd(area_casas$Area), 3))
hist(area_casas$Area, ylab="Frecuencia", xlab="Área",
main = paste("Histograma de las áreas \n  de las estructuras arquitectónicas")) 
text(x = 200, y = 15, cex = 0.8,labels = texto)
#
#  Comparación de los promedios y rangos de error del área de las estructuras según
#  los grupos
#
balasB(area_casas$Grupo, area_casas$Area, xlab= "Grupo", ylab = "Area", an = 1.2)
text(x=1.2,y=180,cex=.7,label="Confianza:  \n  \u2590\u2588\ 80%  \u2588 95%  \u2590 99%", col=1)
#
#  Forma
#
#  Comparacion de las proporciones y los rangos de error de estructuras 
#  arquitectónicas de diferente forma según los grupos
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
#  arquitectónicas con al menos una reconstrucción según los grupos
#
balapropRE(area_casas$Grupo, area_casas$Reconstruida, 2)
#
##