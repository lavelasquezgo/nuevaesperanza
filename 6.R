##
#  Pr?cticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  trav?s de la construcci?n de s?mbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Vel?squez Gonz?lez
#  Universidad Nacional de Colombia
#  2022
#
#  6. K-means
#
#  Adaptado del c?digo de V?ctor Gonz?lez Fern?ndez (2019)
#
#  An?lisis de estructuras arquitect?nicas
#
#  Cargar datos
#
casas_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/6a",
header = TRUE)
#
#  Hacer gr?fico X,Y
#
plot(x = casas_NES$X, y = casas_NES$Y, asp=1)
#
#  Hacer gr?fico de codo
#
mdt_casas <- data.frame(x = casas_NES$X, y = casas_NES$Y)
#
#  Calcular la suma de distancias al cuadrado al interior de un solo grupo
#
wss_casas <- sum(kmeans(mdt_casas,centers=1)$withinss)
wss_casas
#
#  Calcular y a?adir la suma de distancias para 2,3,4,5,6,7,8,9 y 10 grupos 
#
for (i in 1:10) wss_casas[i] <- sum(kmeans(mdt_casas,centers=i)$withinss)
#
wss_casas
#
#  Gr?fico de codo
#
plot(1:10, wss_casas[1:10],type="b",xlab="N?mero de Grupos", ylab="Suma de cuadrados",
     main="Gr?fico de codo de casas", pch=20, cex=2, col = "grey")
abline(v=4, col=2, lty=2)
#
#  An?lisis de K-medias para 4 grupos
#
kc_casas <- kmeans(mdt_casas, 4)
kc_casas
#
#  Gr?fico de los 4 grupos
#
plot(casas_NES$X, casas_NES$Y, pch=19, col = kc_casas$cluster, asp=1)
points(kc_casas$centers, col=1:4, pch=8, cex=2)
#
#  An?lisis de tumbas
#
#  Cargar datos
#
tumbas_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/6b",
header = TRUE)
#
#  Hacer gr?fico X,Y
#
plot(x = tumbas_NES$X, y = tumbas_NES$Y, asp=1)
#
#  Hacer gr?fico de codo
#
mdt_tumbas <- data.frame(x = tumbas_NES$X, y = tumbas_NES$Y)
#
#  Calcular la suma de distancias al cuadrado al interior de un solo grupo
#
wss_tumbas <- sum(kmeans(mdt_tumbas,centers=1)$withinss)
wss_tumbas
#
#  Calcular y a?adir la suma de distancias para 2,3,4,5,6,7,8,9 y 10 grupos 
#
for (i in 1:10) wss_tumbas[i] <- sum(kmeans(mdt_tumbas,centers=i)$withinss)
#
wss_tumbas
#
#  Gr?fico de codo
#
plot(1:10, wss_tumbas[1:10],type="b",xlab="N?mero de Grupos", ylab="Suma de cuadrados",
     main="Gr?fico de codo de tumbas", pch=20, cex=2, col = "grey")
abline(v=4, col=2, lty=2)
#
#  An?lisis de K-medias para 4 grupos
#
kc_tumbas <- kmeans(mdt_tumbas, 4)
kc_tumbas
#
#  Gr?fico de los 4 grupos
#
plot(tumbas_NES$X, tumbas_NES$Y, pch=19, col = kc_tumbas$cluster, asp=1)
points(kc_tumbas$centers, col=1:4, pch=8, cex=2)
#
#  An?lsis de dep?sitos
#
#  Cargar datos
#
depositos_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/6c",
header = TRUE)
#
#  Hacer gr?fico X,Y
#
plot(x = depositos_NES$X, y = depositos_NES$Y, asp=1)
#
#  Hacer gr?fico de codo
#
mdt_depositos <- data.frame(x = depositos_NES$X, y = depositos_NES$Y)
#
#  Calcular la suma de distancias al cuadrado al interior de un solo grupo
#
wss_depositos <- sum(kmeans(mdt_depositos,centers=1)$withinss)
wss_depositos
#
#  Calcular y a?adir la suma de distancias para 2,3,4,5,6,7,8,9 y 10 grupos 
#
for (i in 1:10) wss_depositos[i] <- sum(kmeans(mdt_depositos,centers=i)$withinss)
#
wss_depositos
#
#  Gr?fico de codo
#
plot(1:10, wss_depositos[1:10],type="b",xlab="N?mero de Grupos", ylab="Suma de cuadrados",
     main="Gr?fico de codo de dep?sitos", pch=20, cex=2, col = "grey")
abline(v=4, col=2, lty=2)
#
#  An?lisis de K-medias para 4 grupos
#
kc_depositos <- kmeans(mdt_depositos, 4)
kc_depositos
#
#  Gr?fico de los 4 grupos
#
plot(x = depositos_NES$X, y = depositos_NES$Y, pch=19, col = kc_depositos$cluster, asp=1)
points(kc_depositos$centers, col=1:4, pch=8, cex=2)
#
##