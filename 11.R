##
#  Pr?cticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  trav?s de la construcci?n de s?mbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Vel?squez Gonz?lez
#  Universidad Nacional de Colombia
#  2022
#
#  11. Comparaci?n venado
#
#  Adaptado del c?digo de V?ctor Gonz?lez Fern?ndez (2019)
#
#  Cargar la funci?n balasB
#
source("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/11a")
#
#  Cargar datos de los restos ?seos de fauna asociados a los cortes de 2 m por 2 m
#
fauna <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/11b",
header = TRUE)
fauna <- na.omit(fauna)
#
#  Tabulaci?n
#
tabla_fauna <- with(fauna, table(Grupo, Total_venado))
tabla_fauna
#
#  Tabla de frecuencias
#
frecuencias_venado <- rbind(Grupo1 = sum(tabla_fauna[1,2:15] * as.numeric(dimnames(tabla_fauna)$Total_venado[2:15])), 
Grupo2 = sum(tabla_fauna[2,2:15] * as.numeric(dimnames(tabla_fauna)$Total_venado[2:15])),
Grupo3 = sum(tabla_fauna[3,2:15] * as.numeric(dimnames(tabla_fauna)$Total_venado[2:15])),
Grupo4 = sum(tabla_fauna[4,2:15] * as.numeric(dimnames(tabla_fauna)$Total_venado[2:15])))
frecuencias_venado
#
#  Tabla de proporciones
#
round(prop.table(frecuencias_venado)*100, 2)
#
#  Generar el gr?fico de balas
#
balasB(fauna$Grupo, fauna$Total_venado, xlab="Grupo", ylab="Venado")
#
##