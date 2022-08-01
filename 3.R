##
#  Prácticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  través de la construcción de símbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Velásquez González
#  Universidad Nacional de Colombia
#  2022
#
#  3. Densidad venado Krige
#
#  Adaptado del código de Víctor González Fernández (2019)
#
#  Cargar paquetes
#
library(sp) # coordinates()
library(automap) # autoKrige()
library(raster)  # raster()
#
#  Cargar datos de los restos óseos de fauna asociados a los cortes de 2 m por 2 m
#
fauna <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/3a",
header=TRUE, row.names=1)
str(fauna, list.len = 15)
names(fauna) 
#
#  Crear una nueva variable con el total de restos oseos de venado por corte
#
fauna$venadoT <- fauna$Venado + fauna$Venado_de_cola_blanca + fauna$Venado_rojo
#
#  Crear data frame con datos espaciales y de venado
#
xy_Venado <- data.frame(x=fauna$X, y=fauna$Y, z=fauna$venadoT)
#
#  Mapa de "burbuja"
#
with(xy_Venado,plot(x, y, pch=20, cex=z/3, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Venado) <- ~x+y
spplot(xy_Venado, colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar
#
superf_Venado <- autoKrige(z~1, xy_Venado, block = c(7,7))
#
#  Graficar el resultado
#
plot(superf_Venado)
par(mfrow = c(1,1))
#
#  Extraer el resultado
#
ks <- superf_Venado$krige_output
#
#  Mapa de calor
#
image(ks)
# 
#  Mapa de contornos
#
contour(ks)
#
#  Mapa de calor y de contornos
#
image(ks)
contour(ks, add = T)
#
#  Crear el ráster
#
r <- raster(ks)
x <- rasterToContour(r, maxpixels = 100000, nlevels= 9)
plot(r)
plot(x, add=TRUE)
#
##