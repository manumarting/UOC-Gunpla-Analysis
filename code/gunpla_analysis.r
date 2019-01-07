## Script en R empleado en la práctica 2 de Tipología y ciclo de vida de los datos.

## Luis Manuel Martin Guerra @UOC

## CARGA DE LAS LIBRERÍAS NECESARIAS
library(knitr)
library(nortest)
library(VIM)
library(ggplot2)

# Cargamos el dataset ya pre-procesado
gunpla <- read.table("/Users/manu/Documents/UOC - Ciencia de Datos/2 - Tipología y ciclo de vida de los datos/PRACTICA 2/gunpla_clean.csv", header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)

# Visualizamos los datos
summary(gunpla)

# Obtenemos las variables
var_class <-sapply(gunpla, class)
 
# Mostramos los tipos de datos
kable(data.frame(variable=names(var_class), tipo=as.vector(var_class)))


# Convertimos el Peso a numérico
gunpla[8]<-lapply(gunpla[8], as.numeric)

# Comprobamos que se ha realizado correctamente.
var_class <-sapply(gunpla, class)
kable(data.frame(variable=names(var_class), tipo=as.vector(var_class)))


# Buscamos los valores NULOS
sapply(gunpla, function(x)(sum(is.na(x)))) # NA counts

# Diagramos de caja de las variables estadísticas 
Boxplot(Embalaje~Escala, data=gunpla, id.method="y")

Boxplot(Precio~Escala, data=gunpla, id.method="y")

Boxplot(Peso~Escala, data=gunpla, id.method="y")

# Eliminamos la columna Year
gunpla <- within(gunpla, {Year <- NULL })

# Ordenamos por orden de peso el dataset
gunpla <- with(gunpla, gunpla[order(Peso, decreasing=FALSE), ])

# Cálculo de los valores aproximados basados en los k-vecinos.
gunpla$Embalaje <- kNN(gunpla)$Embalaje


sapply(gunpla, function(x)(sum(is.na(x)))) # NA counts


# Test Shapiro-Wilk para ver la normalización 
shapiro.test(gunpla$Peso)

shapiro.test(gunpla$Precio)

shapiro.test(gunpla$Embalaje)

# Creamos la matriz de correlación

cor(gunpla[,c("Embalaje","Factor_Escala","Peso","Precio")], method="spearman", use="complete")


# Creamos un modelo de regresión
modelo_gunpla <- lm(Precio~Embalaje+Escala+Peso, data=gunpla)
summary(modelo_gunpla)

# Creamos un dataframe para probar el modelo
test_gundam <-data.frame(Nombre="Test", Factor_Escala=4,Escala="1/144",Detalle="High Grade", Serie="Gundam SEED", Embalaje=1600, Peso =140, Precio=5.3)

# Ponemos a prueba el modelo
predict(modelo_gunpla,test_gundam)

# Diagramas de dispersión de la escala, peso, embalaje y precio
ggplot(gunpla, aes(Escala, Peso, color=Escala))+geom_point()

ggplot(gunpla, aes(Precio, Peso, color=Escala))+geom_point()

ggplot(gunpla, aes(Embalaje, Precio, color=Escala))+geom_point() 

# Gráfica de barras con las escalas y el nivel de detalle.
with(gunpla, Barplot(Detalle, by=Escala, style="divided", legend.pos="topright", xlab="Detalle", ylab="Frequency"))
