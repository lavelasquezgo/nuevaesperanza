##
#  Pr?cticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  trav?s de la construcci?n de s?mbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Vel?squez Gonz?lez
#  Universidad Nacional de Colombia
#  2022
#
#  9. Comparaci?n dep?sitos
#
#  Adaptado del c?digo de V?ctor Gonz?lez Fern?ndez (2019)
#
#  Cargar paquetes
#
library(cluster)   # daisy()
library(RcmdrMisc) # rowPercents()
library(vcd) # assocstats()
#
#  Cargar datos
#
depositos <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/9a",
header = TRUE, row.names = 1)
names(depositos)
#
#  An?lisis de K-means multidimensional
#
#  Crear tabla con datos num?ricos
#
depositos_<- subset(depositos, select=7:12)
#
#  Escalar datos
#
depositos_esc <- data.frame(scale(depositos_))
#
#  Matrices de distancia y de similitud
#
(DISTMAT <- daisy(depositos_esc, metric = "euclidean")) # Calcula matriz de distancia
#
#  Evaluar si hay mucha o poca distancia entre casos
#
hist(DISTMAT) # Creamos un histograma y vemos qu? parte pesa m?s. 
              # Si pesan m?s las distancias mayores, est? "distanciado" (mucha distancia)
              # Si pesan las distancias menores, est? "agrupado" (poca distancia)
              # si cumple distribuci?n normal, es aleatorio
#
#  Calcular coeficiente de aglomeraci?n
#
aglomeracion <- agnes(depositos_esc, method = "complete")
cat("el coeficiente de aglomeraci?n es ", aglomeracion$ac, "\n")
#
#  Hacer el dendrograma
#
summary(DISTMAT) # mediana, media y cuartiles de la matriz de distancia
#
hc <- hclust(DISTMAT, method = "complete") # crea datos para construir dendrograma
dendrohc <- as.dendrogram(hc)
#
#  Ver el dendrograma
#
plot(dendrohc) # Plano de dendrograma
abline(h = c(seq(0.01,0.40, by=0.05)), col =c(2:13)) # lineas
#
#  An?lisis de K-means
#
#  Gr?fico de codo
#
wss <- sapply(1:8,
 function(k){kmeans(depositos_esc,k,nstart=10,iter.max=20000)$tot.withinss})
plot(1:8,wss, type="b", pch=19, frame=FALSE, xlab="No. de grupos",
 ylab="Suma de cuadrados en grupos")
abline(v=5, lty=2)
#
#  Revisar la diferencia entre withinss (suma de cuadrados) de 2 a k grupos.
#
#  A menor valor de la suma de cuadrados de los grupos, menos es la
#  diferencia entre los grupos.
# 
#  El estr?s disminuye entre 5 a 11 grupos.
# 
#  A partir de 5 o m?s grupos el cambio no es mayor, es decir no se 
#  reducen mucho las diferencias en los grupos si se crea un mayor 
#  n?mero de categor?as de agurpaci?n.
#
#  K-means para 5 grupos
#
#  Cambiar el n?mero de grupos
#
(k=5) # asigna 5 grupos
#
kc <- kmeans(depositos_esc, k, iter.max = 1000) # crear el kmeans
print(kc) # Imprimir kc
str(kc) # Revisar la estructura del objeto kc
#
#  Asignar la columna de pertencia grupal a la tabla de datos en bruto
#
depositos$Grupo_deposito <- kc$cluster
write.csv(depositos, "grupo_depositos.csv")
#
#  Graficar los 5 grupos al tiempo
#
plot(depositos$X, depositos$Y, col=kc$cluster, pch=20, asp=1) 
#
#  Extraer e imprimir los centroides
#
centroides <- kc$centers
centroides
#
# Plot de cada uno de los grupos
#
Grupo1 <- subset(depositos, Grupo_deposito==1)
with(Grupo1, plot(X, Y, pch=20, xlim=c(976900, 977300), 
ylim = c(997200, 997600), col = 2, asp=1, cex = 1.2))  
kc$centers[1,]
#
Grupo2 <- subset(depositos, Grupo_deposito==2)
with(Grupo2, plot(X, Y, pch=20, xlim=c(976900, 977300), 
ylim = c(997200, 997600), col = "purple", asp=1, cex = 1.2))  
kc$centers[2,]
#
Grupo3 <- subset(depositos, Grupo_deposito==3)
with(Grupo3, plot(X, Y, pch=20, xlim=c(976900, 977300), 
ylim = c(997200, 997600), col = 4, asp=1, cex = 1.2))  
kc$centers[3,]
#
Grupo4 <- subset(depositos, Grupo_deposito==4)
with(Grupo4, plot(X, Y, pch=20, xlim=c(976900, 977300), 
ylim = c(997200, 997600), col = "gold", asp=1, cex = 1.2))  
kc$centers[4,]
#
Grupo5 <- subset(depositos, Grupo_deposito==5)
with(Grupo5, plot(X, Y, pch=20, xlim=c(976900, 977300), 
ylim = c(997200, 997600), col = 5, asp=1, cex = 1.2))  
kc$centers[5,]
#
# Comparaci?n con Chi cuadrado
#
# Crear tabla de grupos de ?rea (1 a 4) por grupos de dep?sitos obtenidos con K-means (1 a 5)
#
tipos_depositos <- read.table(header = TRUE, text = "
Grupo  A     B     C     D       E
1     28     1     5     3     347
2     35     0    12     3     444
3     19     1     6     4     306
4     36     1    14     6     422
", row.names = 1)
#
tipos_depositos <- as.matrix(tipos_depositos)
#
# Crear tabla de proporciones
#
prop_depositos <- rowPercents(tipos_depositos)
prop_depositos
write.csv(prop_depositos, "proporciones_depositos.csv")
#
# Chi cuadrado
#
chisq.test(tipos_depositos)
assocstats(tipos_depositos)
#
##

