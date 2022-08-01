##
#  Prácticas rituales y vida cotidiana. Expresiones de agencia colectiva a
#  través de la construcción de símbolos en la comunidad local de Nueva Esperanza,
#  siglos VI a.C. a XV d.C.
#
#  Laura Velásquez González
#  Universidad Nacional de Colombia
#  2022
#
#  8. Comparación tumbas
#
#  Adaptado del código de Víctor González Fernández (2019)
#
#  Cargar paquetes
#
library(RcmdrMisc)   # rowPercents()
library(vcd)         # assocstats() 
#
#  Cargar datos
#
tumbas_NES <- read.table("https://raw.githubusercontent.com/lavelasquezgo/nuevaesperanza/main/8a",
header = TRUE)
#
tumbas_Chi <- data.frame(tumbas_NES, row.names = TRUE)
#
#  Chi cuadrado tipo de tumba
#
#  Crear tabla de grupos (1 a 4) por tipo de tumba
#
forma_tumba <- table(tumbas_Chi$Grupo, tumbas_Chi$Forma_tumba)
forma_tumba
write.csv(forma_tumba, "forma_tumba.csv")
#
#  Crear tabla de proporciones
#
prop_formatumba <- rowPercents(forma_tumba)
prop_formatumba
write.csv(prop_formatumba, "prop_forma_tumba.csv")
#
#  Chi cuadrado tipo de tumba
#
chisq.test(forma_tumba)
assocstats(forma_tumba)
#
#  Chi cuadrado construcciones internas de la tumba
#
#  Crear tabla de grupos  (1 a 4) por construcciones internas de la tumba
#
construcciones <- table(tumbas_Chi$Grupo, tumbas_Chi$Construcciones_internas)
construcciones
write.csv(construcciones, "construcciones_internas.csv")
#
#  Crear tabla de proporciones
#
prop_construcciones <- rowPercents(construcciones)
prop_construcciones
write.csv(prop_construcciones, "prop_estructuras_internas.csv")
#
#  Chi cuadrado construcciones internas
#
chisq.test(construcciones)
assocstats(construcciones)
#
#  Chi cuadrado orientación de la tumba
#
#  Crear tabla de grupos  por tipo de orientación
#
orientacion <- table(tumbas_Chi$Grupo, tumbas_Chi$Orientacion_tumba)
orientacion
write.csv(orientacion, "orientacion.csv")
#
#  Crear tabla de proporciones
#
prop_orientacion <- rowPercents(orientacion)
prop_orientacion
write.csv(prop_orientacion, "prop_orientacion.csv")
#
#  Chi cuadrado orientación
#
chisq.test(orientacion)
assocstats(orientacion)
#
#  Crear tabla de grupos por número de objetos diferentes (1 a 3) en el ajuar
#
objetos_ajuar <- table(tumbas_Chi$Grupo, tumbas_Chi$Tipo_objetos)
objetos_ajuar
write.csv(objetos_ajuar, "objetos_ajuar.csv")
#
#  Crear tabla de proporciones
#
prop_objetos <- rowPercents(objetos_ajuar)
prop_objetos
write.csv(prop_objetos, "proporciones_objetos_ajuar.csv")
#
#  Chi cuadrado objetos ajuar
#
chisq.test(objetos_ajuar)
assocstats(objetos_ajuar)
#
##