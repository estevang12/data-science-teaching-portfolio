# Taller de Regresion
grasas <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
# Desplegamos los atributos del dataset
names(grasas)
# determinar las relaciones existentes entre cada par de variables
pairs(grasas)
# cuantificar el grado de relación lineal
cor(grasas)
#Calculo y representación de la recta de mínimos cuadrados
regresion <- lm(grasas ~ edad, data = grasas)
#obtenemos un resumen de los principales resultados
summary(regresion)
#´Estimate´ de la tabla ´Coefficients
#  en este ejemplo la ecuación de la recta de mínimos cuadrados es:
# y=102.575+5.321x
# Dibujamos Edad vs Grasa
plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')
# Dibujamos la recta de regresión
abline(regresion)
#Cálculo de predicciones
nuevas.edades <- data.frame(edad = seq(30, 50))
# Queremos usarla recta de mínimos cuadrados para predecir la
#cantidad de grasas para individuos de edades 31,31,32,.,5031,31,32,.,50
#Usamos el fichero de datos que contenga las nuevas variables regresoras y usar el
#comando predict
predict(regresion, nuevas.edades)
#La función confint de R se usa para obtener intevalos de confianza de los
# parámetros de un modelo de regresión lineal
confint(regresion)

#calcula y representa los dos tipos de intervalos para el rango de edades que va de 20
#a 60 años (los de predicción en rojo):
nuevas.edades <- data.frame(edad = seq(20, 60))
# Grafico de dispersion y recta
  plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')
  abline(regresion)
  
  # Intervalos de confianza de la respuesta media:
   # ic es una matriz con tres columnas: la primera es la prediccion, las
  #otras dos son los extremos del intervalo
  ic <- predict(regresion, nuevas.edades, interval = 'confidence')
  lines(nuevas.edades$edad, ic[, 2], lty = 2)
  lines(nuevas.edades$edad, ic[, 3], lty = 2)

  # Intervalos de prediccion
  ic <- predict(regresion, nuevas.edades, interval = 'prediction')
  lines(nuevas.edades$edad, ic[, 2], lty = 2, col = 'red')
  lines(nuevas.edades$edad, ic[, 3], lty = 2, col = 'red')
  
 # Diagnóstico del modelo
  residuos <- rstandard(regresion)
  valores.ajustados <- fitted(regresion)
  plot(valores.ajustados, residuos)
  
  qqnorm(residuos)
  qqline(residuos)
  
