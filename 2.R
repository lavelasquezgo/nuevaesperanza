##
#  Prácticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  través de la construcción de símbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Velásquez González
#  Universidad Nacional de Colombia
#  2022
#
#  2. Formas cerámicas Krige
#
#  Adaptado del código de Víctor González Fernández (2019)
#
#  Cargar paquetes
#
library(sp) # coordinates()
library(automap) # autoKrige
library(raster)  # raster()
#
#  Cargar datos de fragmentos cerámicos diagnósticos asociados
#  a los cortes de 2 m por 2 m
#
densidad <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/densidad_ceramica",
header=TRUE, row.names=1)
str(densidad, list.len = 15)
names(densidad)
#
#  Copas
#
#  Crear data frame con datos espaciales y de copas
#
xy_Copa <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Copa)
#
#  Mapa de "burbuja"
#
with(xy_Copa,plot(x,y,pch=20,cex=0.01+z/3, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Copa) <- ~x+y
spplot(xy_Copa,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar con Krige
#
superf_Copa <- autoKrige(z~1, xy_Copa)
#
#  Graficar el resultado
#
plot(superf_Copa)
#
#  Extraer el resultado
#
ks <- superf_Copa$krige_output
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
#  Jarras
#
#  Crear data frame con datos espaciales y de jarras
#
xy_Jarra <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Jarra)
#
#  Mapa de "burbuja"
#
with(xy_Jarra,plot(x,y,pch=20,cex=z/3, asp=1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Jarra) <- ~x+y
spplot(xy_Jarra,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar con Krige
#
superf_Jarra <- autoKrige(z~1, xy_Jarra)
#
#  Graficar el resultado
#
plot(superf_Jarra)
#
#  Extraer el resultado
#
ks <- superf_Jarra$krige_output
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
#  Cuencos
#
#  Crear data frame con datos espaciales y de cuencos
#
xy_Cuenco <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Cuenco)
#
#  Mapa de "burbuja"
#
with(xy_Cuenco,plot(x,y,pch=20,cex=z/3, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Cuenco) <- ~x+y
spplot(xy_Cuenco,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar con Krige
#
superf_Cuenco <- autoKrige(z~1, xy_Cuenco)
#
#  Graficar el resultado
#
plot(superf_Cuenco)
#
#  Extraer el resultado
#
ks <- superf_Cuenco$krige_output
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
#  Ollas
#
#  Crear data frame con datos espaciales y de ollas
#
xy_Olla <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Olla)
#
#  Mapa de "burbuja"
#
with(xy_Olla,plot(x,y,pch=20,cex=z/3, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Olla) <- ~x+y
spplot(xy_Olla,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar con Krige
#
superf_Olla <- autoKrige(z~1, xy_Olla)
#
#  Graficar el resultado
#
plot(superf_Olla)
#
#  Extraer el resultado
#
ks <- superf_Olla$krige_output
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