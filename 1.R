##
#  Prácticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  través de la construcción de símbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Velásquez González
#  Universidad Nacional de Colombia
#  2022
#
#  1. Densidad cerámica Krige
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
#  a los cortes de 2 m por 2 m.
#
densidad <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/1a",
header=TRUE, row.names=1)
str(densidad, list.len = 15)
names(densidad)
#
#  Interpolación de Krige periodo Herrera 
#
xy_Herrera <- data.frame(x=densidad$X, y=densidad$Y, z=densidad$Herrera_total_ponderado)
#
#  Mapa de "burbuja"
#
with(xy_Herrera, plot(x, y, pch=20, cex=z*60, asp=1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Herrera) <- ~x+y
spplot(xy_Herrera,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar
#
superf_Herrera <- autoKrige(z~1, xy_Herrera)
#
#  Graficar el resultado
#
plot(superf_Herrera)
#
#  Extraer el resultado
#
ks <- superf_Herrera$krige_output
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
#  Crear el raster
#
r <- raster(ks)
x <- rasterToContour(r, maxpixels = 100000, nlevels= 9)
plot(r)
plot(x, add=TRUE)
#
#  Interpolación de Krige periodo Muisca Temprano 
#
xy_Temprano <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Temprano_total_ponderado)
#
#  Mapa de "burbuja"
#
with(xy_Temprano,plot(x,y,pch=20,cex=z*60, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Temprano) <- ~x+y
spplot(xy_Temprano,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar
#
superf_Temprano <- autoKrige(z~1, xy_Temprano)
#
#  Graficar el resultado
#
plot(superf_Temprano)
#
#  Extraer el resultado
#
ks <- superf_Temprano$krige_output
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
#  Crear el raster
#
r <- raster(ks)
x <- rasterToContour(r, maxpixels = 100000, nlevels= 9)
plot(r)
plot(x, add=TRUE)
#
#  Interpolación de Krige periodo Muisca Tardio
#
xy_Tardio <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Tardio_total_ponderado)
#
#  Mapa de "burbuja":
#
with(xy_Tardio,plot(x,y,pch=20,cex=z*60, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Tardio) <- ~x+y
spplot(xy_Tardio,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar
#
superf_Tardio <- autoKrige(z~1, xy_Tardio)
#
#  Graficar el resultado
#
plot(superf_Tardio)
#
#  Extraer el resultado
#
ks <- superf_Tardio$krige_output
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
contour(ks)
#
#  Crear el raster
#
r <- raster(ks)
x <- rasterToContour(r, maxpixels = 100000, nlevels= 9)
plot(r)
plot(x, add=TRUE)
#
#  Interpolación de Krige periodo Colonial
#
xy_Colonial <- data.frame(x=densidad$X,y=densidad$Y,z=densidad$Colonial_total_ponderado)
#
#  Mapa de "burbuja"
#
with(xy_Colonial,plot(x,y,pch=20,cex=.001+z*60, asp = 1))
#
#  Convertir al tipo "sp" espacial
#
coordinates(xy_Colonial) <- ~x+y
spplot(xy_Colonial,colorkey=TRUE, scales=list(TRUE))
#
#  Interpolar
#
superf_Colonial <- autoKrige(z~1, xy_Colonial)
#
#  Graficar el resultado
#
plot(superf_Colonial)
#
#  Extraer el resultado
#
ks <- superf_Colonial$krige_output
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
#  Crear el raster
#
r <- raster(ks)
x <- rasterToContour(r, maxpixels = 100000, nlevels= 9)
plot(r)
plot(x, add=TRUE)
#
##