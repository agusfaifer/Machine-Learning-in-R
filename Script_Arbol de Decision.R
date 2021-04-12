#ARBOL DE DECISION


#1.1) INSTALACION DE LIBRERIAS
install.packages(c("rpart", "rattle", "rpart.plot", "RColorBrewer"))


# 1.2) IMPORTACION DE  LIBRERIAS

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# 1.3) DATOS DE EJEMPLO
# kyphosis es un dataset que tiene informacion para predecir un tipo de
# deformacion luego de una cirugia.
datosKypo <- kyphosis

# Miramos los primeros datos para entender el dataframe
# Muestra: la edad en meses (age), la cantidad de vertebras involucradas (number),
# y la vertebra operada mas alta (start).
head(datosKypo)


# 2) CREAMOS EL ARBOL DE CLASIFICACION
# Arbol de clasificacion, donde clasificamos el diagnostico de kyposis, respecto a
# la edad, vertebras involucradas y operadas.
fitKypo <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, method = "class")

fitKypo

# 3.1) VISUALIZACION DEL ARBOL GENERADO
# Intentamos visualizar con las herramientas proporcionadas por rpart:
# Miremos resumenes de texto del arbol
printcp(fitKypo)
summary(fitKypo)


# El grafico de esta funcion nos muestra el error relativo para cada "split"
# (o particion, o rama) que se le hizo al Arbol.
rsq.rpart(fitKypo)


# Ahora, armemos el grafico usando las funciones de rpart
plot(fitKypo) # Mostramos las ramas del Arbol
text(fitKypo) # Y luego les agregamos texto

# Podemos intentar mostrar ambos graficos juntos
par(mfrow = c(1,2)) # Ambos graficos, uno al lado del otro
plotcp(fitKypo) # Mostramos primero el error relativo
plot(fitKypo, uniform = TRUE, main = "datosKypo") # Graficamos el Arbol
text(fitKypo, use.n = TRUE, all = TRUE, cex = .7) # Y agregamos texto
par(mfrow = c(1,1)) # Y quitamos el "lado a lado" para graficos futuros


# 3.2) VISUALIZACION MEJORADA
# Usamos las librerias adicionales para crear una mejor visualizacion
?fancyRpartPlot # Para ver la ayuda de la funcion
fancyRpartPlot(fitKypo) # Grafico mejorado


# 4) PODAMOS EL ARBOL Y VOLVEMOS A GRAFICAR
# Vamos a podar el Arbol, quitando aquellos valores donde el error
# de validacion cruzada sea mayor al minimo establecido por el parametro
podaKypo <- prune(fitKypo, 
                  cp = fitKypo$cptable[which.min(fitKypo$cptable[,"xerror"]),
                                       "CP"])
# Visualicemos la poda, y nos damos cuenta que cortamos todas las ramas
# y que el "Arbol" es solo un nodo raiz.
# No siempre resulta conveniente realizar podas!
printcp(podaKypo)



