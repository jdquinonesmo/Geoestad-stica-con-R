#  Estudio del libro "An introduction to (geo)statistics with R" 
#  D G Rossiter Cornell University, Section of Soil & Crop Sciences


install.packages(c("sp", "gstat"), dependencies=TRUE)  # instalación de paquetes
library(sp)
library(gstat)
library(tidyverse)
data(package="sp")
data("meuse")
View(meuse)
help(meuse)

##----------------------------------------------------------------------------##
# BÁSICOS EN ESTADÍSTICA 


hist(meuse$zinc, breaks = 16)
rug(meuse$zinc)
summary(meuse$zinc)

hist(meuse$elev)
rug(meuse$elev)
summary(meuse$ele)

# Correlación entre dos variables "scatter plot"
meuse<-mutate(meuse, log_cu=log10(meuse$copper), log_zn=log10(meuse$zinc))
plot(meuse$log_cu ~ meuse$log_zn)
pairs(data=meuse, ~ zinc+copper+cadmium + elev + lead)

#encontrando índices (filas) en la matriz de valores particulares
#En gráfica log_cu vs log_zn se observan 4 valores alejados de la tendencia lineal

indices<-which(meuse$log_zn<2.5 & meuse$log_cu>1.6); indices

meuse[indices,]
plot(log(meuse$elev) ~ log(meuse$dist.m))
ls()

#use distance in meters from the river as the continuous predictor of topsoil 
#log10Zn content.  Does your analysis give
#support to this hypothesis? Are there any particularly poorly-modelled 
#observations? Do they have a large influence on the model coefficient?

#What is the hypothesis for this model?  R: Bi son diferentes de cero?

pairs(meuse$log_zn~meuse$dist.m) #Hay un relación decreciente ente log_zn y la distancia. 
#Esa relación no es tan lineal así que se podría transformar la distancia con log. 
meuse<-mutate(meuse, log_dist.m=log10(dist.m))
pairs(meuse$log_zn~meuse$log_dist.m)
pairs(meuse$zinc ~ meuse$dist.m)

m.lzn.ldis.m <- lm(log_zn ~ log_dist.m, data=meuse)
summary(m.lzn.ldis.m) 

#el modelo tiene una pobre explicabilidad de la 
#variabilidad de logZn en función de dist.m (60.58%)
#sin embargo, es evidente que hay una relación y que los Bi's son diferentes de 0.
hist(residuals(m.lzn.ldis.m))
par(mfrow=c(1,3))
plot(m.lzn.ldis.m, which=c(1,2,5))
par(mfrow=c(1,1))


plot(meuse$log_zn ~ meuse$log_dist.m, asp=1, col=meuse$ffreq, pch=20,
     xlab="log10(dist.m)", ylab="log10(Zn ppm)")
abline(m.lzn.ldis.m)
legend("topleft", legend=c("2 years","10 years", "50 years"),
       pch=20, col=1:3)

#El modelo tiene un pobre desempeño, teniendo en cuenta sólo la variable dist.m
#Existe una variabilidad alta que podría explicarse con ayuda de otras variables más


#Analizando categóricas
boxplot(meuse$log_zn ~ meuse$ffreq, xlab="Flood frequency class",
        ylab="log10-Zn ppm",
        main="Metal concentration per flood frequency class",
        boxwex=0.5, col="lightblue")

boxplot(meuse$log_cu ~ meuse$ffreq, xlab="Flood frequency class",
        ylab="log10-cu ppm",
        main="Metal concentration per flood frequency class",
        boxwex=0.5, col="lightblue")


boxplot(meuse$log_zn ~ meuse$soil, xlab="Soil type",
        ylab="log10-Zn ppm",
        main="Metal concentration per Soil type",
        boxwex=0.5, col="lightblue")

m.lzn.st <- lm(log_zn ~ soil, data=meuse)
summary(m.lzn.st)

#la concentración promedio en el suelo tipo 1 es de: 2.6857 log10(ppm)
#la con. prom. en el suelo tipo 2 es de: 2.6857+(-0.31059) = 2.37511 log10(ppm)
#la con. prom. en el suelo tipo 3 es de: 2.6857+(-0.48263) = 2.20307 log10(ppm)
#la variabiliad en log_zn explicada por el tipo de suelo es el 29.69%

#relación entre dos variables categóricas
a<-table(meuse$soil, meuse$ffreq)
b<-table(meuse$soil, meuse$lime)

chisq.test(b)


#modelo con interacciones 

with(meuse, plot(log_zn ~ log_cu, col=ffreq, pch = 20,
                 xlab = "log10(Cu)", ylab = "log10(Zn)"))
legend("topleft", legend=levels(meuse$ffreq), pch=20,
       col=1:3)
title(main = "Relation between log10(Zn) and log10(Cu)")
title(sub =
        "Interaction: solid lines; per class; single: dashed line")
abline(lm(log_zn ~ log_cu, data = meuse),col = "purple",
       lty=3, lwd=2.5)
abline(lm(log_zn ~ log_cu, data = meuse,
          subset=(meuse$ffreq==1)),col = 1)
abline(lm(log_zn ~ log_cu, data = meuse,
          subset=(meuse$ffreq==2)),col = 2)
abline(lm(log_zn ~ log_cu, data = meuse,
          subset=(meuse$ffreq==3)),col = 3)

m.lzn.ff.lcu.i <- lm(log_zn ~ ffreq * log_cu, data=meuse)
m.lzn.lcu <-lm(log_zn ~ log_cu, data=meuse)
anova(m.lzn.ff.lcu.i, m.lzn.lcu)

#estadísticamente hay una mejora del modelo al agregar interacciones, P=0.04278
#Pero el % de variabilidad explicado en la práctica sólo aumenta un poco, así 
#que es suficiente explicar la concentración de Zn con la concentración de Cu medida. 


### -------------------------------------------------------------------------###

# OBJETOS ESPACIALMENTE EXPLÍCITOS EN R 

class(meuse) # ver qué clase de objeto es "meuse"
coordinates(meuse) <- c("x","y") #convierte a "meuse" en un objeto espacial
class(meuse)
str(meuse) #para ver la estructura de la clase SpatialPointsDataFrame

plot(meuse, asp=1, pch=1) #dibuja los puntos en donde se realizaron mediciones
data(meuse.riv) #llama el dataset de puntos que dibujan el rio Meuse
lines(meuse.riv) #dibuja las lineas del rio usando el dataset meuse.riv


data(meuse.grid) #llama el dataset meuse.grid
class(meuse.grid)
coordinates(meuse.grid) <-c ("x","y")
gridded(meuse.grid) <- TRUE; fullgrid <- TRUE
class(meuse.grid)
spplot(meuse.grid)

#Árboles de regresión 
library(rpart)
library(rpart.plot)

m.lzn.rp <- rpart(log_zn ~ ffreq + dist + elev,
                  data=meuse,
                  minsplit=2,
                  cp=0.003)  #árbol de regresión con cantidad mínima de 2 obs. 
                             #en cada rama y una mejora mínima de 0.3% en R2 con
                             #cada división que se haga

print(m.lzn.rp)
rpart.plot(m.lzn.rp, digits=3, type=4, extra=1)
?rpart.object
sum(m.lzn.rp$frame$var == '<leaf>')

x <- m.lzn.rp$variable.importance
data.frame(variableImportance = 100 * x / sum(x))
printcp(m.lzn.rp)
plotcp(m.lzn.rp)
head(cp.table <- m.lzn.rp[["cptable"]],8)
(cp.ix <- which.min(cp.table[,"xerror"])) #Encontrando el mínimo error de validación
                                          #cruzada. 
print(cp.table[cp.ix,])

(cp.min.plus.sd <- cp.table[cp.ix,"xerror"] + cp.table[cp.ix,"xstd"])
[1] 0.310379
cp.ix.sd <- min(which(cp.table[,"xerror"] < cp.min.plus.sd)) #encuentra el error
#en la validación cruzada inmediatamente menor al xerror + una desvición estándar
#por encima
print(cp.table[cp.ix.sd,])
cp.min <- cp.table[cp.ix,"CP"]
#ahora se va a construir un árbol podado en el mínimo valor cp dado por el mínimo
#xerror, es decir el cp correspondiente el mínimo xerror.

(m.lzn.rpp <- prune(m.lzn.rp, cp=cp.min))
rpart.plot(m.lzn.rpp, digits=3, type=4, extra=1)
mean(meuse$log_zn)
sum(m.lzn.rpp$frame$var == '<leaf>')
sum(m.lzn.rpp$frame$var != '<leaf>')
m.lzn.rpp$variable.importance

#El árbol podado tiene 10 salidas, es decir 10 valores predichos de logZn 

p.rpp <- predict(m.lzn.rpp, newdata=meuse) #se guardan los valores predichos al dataframe
length(unique(p.rpp)) #aquí se ve que son 10 los valores diferentes, las 10 salidas del árbol podado

summary(r.rpp <- meuse$log_zn - p.rpp)
sqrt(sum(r.rpp^2)/length(r.rpp))
plot(meuse$log_zn ~ p.rpp, asp=1, pch=20, xlab="fitted", ylab="actual",
     xlim=c(2,3.3), ylim=c(2,3.3),
     main="log10(Zn), Meuse topsoils, Regression Tree")
grid()
abline(0,1)

#Predecir sobre el área de estudio con árboles de regresión
#en el objeto de meuse.grid  sólo se tiene la distancia normalizada, no se tiene la elevación
#debemos ingresar los datos de elevación al grid para poder usar el modelo m.lzn.rpp 
#ya que este modelo usó como variables significativas las distancias normalizadas 
#y las elevaciones. Para llenar todas las áreas del grid con las elevaciones 
#toca usar las elevaciones realmente medidas e interpolar las zonas no medidas para
#completar esas áreas o cajas del grid. 

#Predicción de elevaciones sobre el grid por interpolación de la inversa de la distancia^2




