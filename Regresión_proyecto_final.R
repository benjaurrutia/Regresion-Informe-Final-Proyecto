library(rio)
auto.mpg = autos_reg
auto.mpg

attach(auto.mpg)
library(ggplot2)
library(GGally)
library(psych)
library(plotly)

# Limpiando la base de datos ----------------------------------------------

#Datos faltantes 
cars_data<-data.frame(auto.mpg[auto.mpg$horsepower!="?", ] )

#Verificando que sean class= "numeric"

for (i in 1:8){                             
  cars_data[,i]= as.numeric(cars_data[,i]) }     


# Resumen de info ---------------------------------------------------------
summary(cars_data)
datos_limpios<-cars_data



# Regresión múltiple ------------------------------------------------------

datos_limpios2 = datos_limpios[,1:8]
full.lin.mod = lm(datos_limpios2$mpg ~ ., data = datos_limpios2)

lin.mod.1 = full.lin.mod
lin.mod.2 = lm(datos_limpios2$mpg ~ datos_limpios2$displacement + datos_limpios2$weight +
                 datos_limpios2$model.year + 
                 datos_limpios2$origin,data = datos_limpios2)

lin.mod.3 = lm(datos_limpios2$mpg ~   datos_limpios2$weight +
                 datos_limpios2$model.year + 
                 datos_limpios2$origin,data = datos_limpios2)

summary(lin.mod.1)
summary(lin.mod.2)
summary(lin.mod.3)

step(lm(datos_limpios2$mpg ~ 1, data = datos_limpios2), direction = "forward",
     scope = datos_limpios2)

step(lin.mod.1, direction = "backward")
step(lin.mod.3, direction = "both")



#GR?FICOS DE VARIABLES (barras y boxplot)

#MPG
ggplot(datos_limpios, aes(datos_limpios[,1])) + geom_boxplot(colour = "black", fill= "cadetblue3", outlier.colour = "red") +
  labs(title = "Boxplot de MPG", x = "MPG") + coord_flip()

ggplot(datos_limpios, aes(datos_limpios[,1])) + geom_histogram(fill = "cadetblue3", color = "black", binwidth = 2) +
  labs(title = "Histograma de MPG", x ="MPG", y = "Frecuencia")

#CILINDROS
boxplot(cylinders, main = "Boxplot Cilindros",  ylab = "Cilindros", col = "brown1")
ggplot(datos_limpios, aes(datos_limpios[,2])) + geom_bar(fill="brown1") + labs(x = "Cilindros",y = "Frecuencia")+
  ggtitle("Cantidad de Cilindros")

#DESPLAZAMIENTO (cilindrada)
ggplot(datos_limpios, aes(datos_limpios[,3])) + geom_boxplot(colour = "black", fill= "burlywood", outlier.colour = "red") +
  labs(title = "Boxplot de Desplazamiento", x = "Desplazamiento") + coord_flip()

ggplot(datos_limpios, aes(datos_limpios[,3])) + geom_histogram(fill = "burlywood", color = "black", binwidth = 30) +
  labs(title = "Histograma de Desplazamiento", x ="Desplazamiento",y = "Frecuencia")

#CABALLOS DE FUERZA
ggplot(datos_limpios, aes(as.numeric(datos_limpios[,4]))) + geom_boxplot(colour = "black", fill= "darkseagreen3", outlier.colour = "red") +
  labs(title = "Boxplot Caballos de Fuerza", x = "Caballos de fuerza") +
  coord_flip()
ggplot(datos_limpios, aes(as.numeric(datos_limpios[,4]))) + geom_histogram(fill = "darkseagreen3", color = "black", binwidth =15) +
  labs(title = "Histograma de Caballos de Fuerza", x ="Caballos de Fuerza",  y= "Frecuencia")

#PESO
ggplot(cars_data, aes(horsepower)) + geom_boxplot(colour = "black", fill= "cadetblue3", outlier.colour = "red") +
  labs(title = "Boxplot Peso", x = "Peso", y= "Frecuencia") + coord_flip()


ggplot(datos_limpios, aes(as.numeric(datos_limpios[,5]))) + geom_histogram(fill = "cadetblue3", color = "black", binwidth =200) +
  labs(title = "Histograma de Peso", x ="Peso",  y = "Frecuencia")

#ACELERACI?N
ggplot(datos_limpios, aes(as.numeric(datos_limpios[,6]))) + geom_boxplot(colour = "black", fill= "burlywood", outlier.colour = "red") +
  labs(title = "Boxplot Aceleración", x = "Aceleración") + coord_flip()

ggplot(datos_limpios, aes(as.numeric(datos_limpios[,6]))) + 
  geom_histogram(fill = "burlywood", color = "black", binwidth =1) +
  labs(title = "Histograma de Aceleraci?n", x ="Aceleraci?n", y = "Frecuencia")

# A?O MODELO
ggplot(datos_limpios, aes(as.numeric(datos_limpios[,7]))) + geom_boxplot(colour = "black", fill= "burlywood", outlier.colour = "red") +
  labs(title = "Boxplot año del Modelo", x = "Año") + coord_flip()

ggplot(datos_limpios, aes(datos_limpios[,7])) + geom_bar(fill = "coral", color = "black") + labs(x = "Cilindros",y = "Frecuencia")+
  labs(title = "Gráfico de barras del año del Modelo", x ="Año")

#ORIGEN DE FABRICACI?N 

## Cambiando la variable "origin" numeric a string
origins <- c("USA", "Europa", "Jap?n")
cars_data$origin =factor(cars_data$origin, labels = origins)

ggplot(cars_data, aes(origin))+ geom_bar(aes(fill = origin))+ 
  ggtitle("Origen de fabricaci?n") + labs(x="Origen",y= "Frecuencia")


# Correlaci?n entre variables ---------------------------------------------

#Correlaci?n con variable respuesta
correlaciones<-NULL
for (i in 1:8){
  
  correlaciones[i] = cor(cars_data[,1] , as.numeric(cars_data[,i])) 
}

correlaciones

ggcorr(datos_limpios[1:8], palette = "PuOr", label = T,digits = 1,label_round = 2,
       name = expression(rho),
       max_size =10,
       min_size = 2) + ggtitle("Correlaciones entre las variables ")

#GRAFICOS DE CORRELACION Y DISPERSION
ggpairs(cars_data[,1:6], mapping = aes(color = cars_data$origin), columnLabels =c("MPG", "Cilindros","Desplazamiento", "Caballos de fuerza", "Peso", "Aceleraci?n"))


pairs.panels(cars_data[,1:6], method = "pearson",
             hist.col = "cyan", ellipses = FALSE,lm=TRUE, 
             factor=3, stars = TRUE,
             labels = c("MPG", "Cilindros","Desplazamiento", "Caballos de fuerza", "Peso", "Aceleraci?n"),
             main = "Gr?fico de Dispersi?n y correlaci?n",
             lwd=2 )


# Regresi?n lineal simple de la variable con mayor cor() con mpg ----------

regresion = lm(mpg~weight, data = cars_data) #coeficiente e intercepto
summary(regresion)

# gr?fico con regresion lineal
ggplot(cars_data, aes(weight, mpg)) + 
  geom_point() + labs(x="Peso", y="Millas por gal?n")+ 
  geom_line(aes(y=predict(regresion)), color = "purple", lwd=1)+
  ggtitle("Regresi?n lineal simple")+ theme_minimal()

# gr?fico separado por origen
ggplot(cars_data, aes(weight, mpg)) + 
  geom_point(aes(color = origin))+ theme_minimal()+
  labs(x="Peso", y="Millas por gal?n")+
  geom_line(aes(y=predict(regresion)), color = "purple", lwd=1)+
  ggtitle("Regresi?n lineal simple")

ggplot(cars_data, aes(weight, mpg)) + 
  geom_point(aes(color = origin))+ theme_minimal()+
  labs(x="Peso", y="Millas por gal?n")+
  geom_line(aes(y=predict(regresion)), color = "purple", lwd=1)+
  ggtitle("Regresi?n lineal simple")+
  facet_grid(~ cars_data$origin)


