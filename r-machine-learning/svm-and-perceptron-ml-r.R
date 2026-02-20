library(ggplot2)
set.seed(68)
X1 <- rnorm(n = 10, mean = 2, sd = 1)
X2 <- rnorm(n = 10, mean = 2, sd = 1)

observaciones <- data.frame(X1 = c(X1, X1 + 2), X2 = c(X2, X2 + 2) ,
                            clase = rep(c(1, -1), each = 10))
observaciones$clase <- as.factor(observaciones$clase)

ggplot() +
  geom_point(data = observaciones, aes(x = X1, y = X2, color = clase), size = 4) +
  geom_abline(intercept = 9, slope = -2) +
  geom_abline(intercept = 8.5, slope = -1.7) +
  geom_abline(intercept = 8, slope = -1.5) +
  geom_abline(intercept = 6.5, slope = -1) +
  geom_abline(intercept = 5.4, slope = -0.75) +
  theme_bw() +
  labs(title = "5 Posibles hiperplanos de separación") +
  theme( legend.position = "none",
         plot.title = element_text(hjust = 0.5, size = 11))

#CASOS CUASI-SEPARABLES LINEALMENTE
set.seed(101)
coordenadas <- matrix(rnorm(40), 20, 2)
colnames(coordenadas) <- c("X1","X2")
y <- c(rep(-1,10), rep(1,10))
y <- as.factor(y)
coordenadas[y == 1, ] <- coordenadas[y == 1, ] + 1

datos <- data.frame(coordenadas, y)
ggplot(data = datos, aes(x = X1, y = X2, color = as.factor(y))) +
  geom_point(size = 4) +
  theme_bw() +
  labs(title = "Clases no separables linealmente") +
  theme( legend.position = "none",
         plot.title = element_text(hjust = 0.5, size = 11))

#EJEMPLO
set.seed(10111)
coordenadas <- matrix(rnorm(40), 20, 2)
colnames(coordenadas) <- c("X1","X2")
y <- c(rep(-1,10), rep(1,10))
coordenadas[y == 1, ] <- coordenadas[y == 1, ] + 1
datos <- data.frame(coordenadas, y)
ggplot(data = datos, aes(x = X1, y = X2, color = as.factor(y))) +
  geom_point(size = 6) +
  theme_bw() +
  theme(legend.position = "none")
# SE CONVIERTE A FACTOR
library(e1071)

# Se convierte la variable respuesta a factor
datos$y <- as.factor(datos$y)

# Para que la función svm() calcule el Support Vector Classifier,
# se tiene que indicar que la función kernel es lineal.
modelo_svm <- svm(formula = y ~ X1 + X2, data = datos, kernel = "linear",
                  cost = 10, scale = FALSE)
summary(modelo_svm)

# Índice de las observaciones que actúan como vector soporte
modelo_svm$index

plot(modelo_svm, datos)

# SI AL AJUSTAR EL MODELO SE INDICA scale = true, SE TIENEN QUE ESTANDARIZAR
# TAMBIÉN LAS OBSERVACIONES PARA QUE COINCIDAN LAS COORDENADAS.

# Se interpolar puntos dentro del rango de los dos predictores X1 y X2.
# Estos nuevos puntos se emplean para predecir la variable respuesta acorde
# al modelo y así colorear las regiones que separa el hiperplano.

# Rango de los predictores
rango_X1 <- range(datos$X1)
rango_X2 <- range(datos$X2)

# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 75)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 75)
nuevos_puntos <- expand.grid(X1 = new_x1, X2 = new_x2)

# Predicción según el modelo
predicciones <- predict(object = modelo_svm, newdata = nuevos_puntos)

# Se almacenan los puntos predichos para dar color a las regiones
color_regiones <- data.frame(nuevos_puntos, y = predicciones)

# Para extraer la ecuación del hiperplano y del margen es necesario aplicar 
# algebra lineal.
beta <- drop(t(modelo_svm$coefs) %*% as.matrix(datos[,c("X1","X2")])[modelo_svm$index,])
beta0 <- modelo_svm$rho


ggplot() +
  # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = color_regiones, aes(x = X1, y = X2, color = as.factor(y)),
             size = 0.5) +
  # Se añaden las observaciones
  geom_point(data = datos, aes(x = X1, y = X2, color = as.factor(y)),
             size = 6) +
  # Se identifican aquellas observaciones que son vectores soporte del modelo
  geom_point(data = datos[modelo_svm$index, ],
             aes(x = X1, y = X2, color = as.factor(y)),
             shape = 21, colour = "black",
             size = 6) +
  # Se añaden las rectas del hiperplano y los márgenes
  geom_abline(intercept = beta0/beta[2], slope = -beta[1]/beta[2]) +
  geom_abline(intercept = (beta0 - 1)/beta[2], slope = -beta[1]/beta[2],
              linetype = "dashed") +    
  geom_abline(intercept = (beta0 + 1)/beta[2], slope = -beta[1]/beta[2],
              linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none")

##
set.seed(1)
svm_cv <- tune("svm", y ~ X1 + X2, data = datos,
               kernel = 'linear',
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
                                      150, 200)))
summary(svm_cv)


ggplot(data = svm_cv$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

mejor_modelo <- svm_cv$best.model
# PREDICCION
# Datos de test simulados
set.seed(19)
coordenadas <- matrix(rnorm(40), 20, 2)
colnames(coordenadas) <- c("X1","X2")
y <- sample(c(-1,1), 20, rep = TRUE)
coordenadas[y == 1, ] <- coordenadas[y == 1, ] + 1
test <- data.frame(coordenadas, y)

# Predicciones
predicciones <- predict(object = mejor_modelo, test)
paste("Error de test:", 100*mean(test$y != predicciones),"%")

table(predicción = predicciones, valor_real = test$y)

#Ejemplo

# Se eliminan todas las variables para evitar conflictos
rm(list = ls())
# Descargan los datos. Requiere conexión a internet
load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))

datos <- data.frame(ESL.mixture$x, y = ESL.mixture$y)
# Al tratarse de un problema de clasificación se convierte la variable
# respuesta en factor
datos$y <- as.factor(datos$y)
head(datos)

ggplot(data = datos, aes(x = X1, y = X2, color = y)) +
  geom_point(size =2.5) +
  theme_bw() +
  theme(legend.position = "none")
#Identificación de hiperparámetros óptimos y ajuste del modelo.
library(e1071)
# Como los datos se han simulado en una misma escala, no es necesario estandarizarlos
# si no fuese así, es muy importante hacerlo.
set.seed(1)
svm_cv <- tune("svm", y ~ X1 + X2, data = datos, kernel = 'radial',
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20),
                             gamma = c(0.5, 1, 2, 3, 4, 5, 10)))

ggplot(data = svm_cv$performances, aes(x = cost, y = error, color = as.factor(gamma)))+
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetros C y gamma", color = "gamma") +
  theme_bw() +
  theme(legend.position = "bottom")

svm_cv$best.parameters

modelo_svm_rbf <- svm_cv$best.model

#Representación gráfica del clasificador SVM.

# Se interpolar puntos dentro del rango de los dos predictores X1 y X2.
# Estos nuevos puntos se emplean para predecir la variable respuesta acorde
# al modelo y así colorear las regiones que separa el hiperplano.

# Rango de los predictores
rango_X1 <- range(datos$X1)
rango_X2 <- range(datos$X2)

# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 75)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 75)
nuevos_puntos <- expand.grid(X1 = new_x1, X2 = new_x2)

# Predicción según el modelo de los nuevos puntos
predicciones <- predict(object = modelo_svm_rbf, newdata = nuevos_puntos)

# Se almacenan los puntos predichos para el color de las regiones en un dataframe
color_regiones <- data.frame(nuevos_puntos, y = predicciones)

ggplot() +
  # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = color_regiones, aes(x = X1, y = X2, color = as.factor(y)),
             size = 0.5) +
  # Se añaden las observaciones
  geom_point(data = datos, aes(x = X1, y = X2, color = as.factor(y)),
             size = 2.5) +
  # Se identifican aquellas observaciones que son vectores soporte
  geom_point(data = datos[modelo_svm_rbf$index, ],
             aes(x = X1, y = X2, color = as.factor(y)),
             shape = 21, colour = "black",
             size = 2.5) +
  theme_bw() +
  theme(legend.position = "none")

#Support Vector Machines para más de dos clases
#Ejemplo
library(ISLR)
data("Khan")
names(Khan)
dim(Khan$xtrain)

dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)

#grafico
library(e1071)

# Como la variable respuesta está separa de los predictores, se unen en un único 
# dataframe. La variable respuesta tiene que ser de tipo factor.
datos_train <- data.frame( y = as.factor(Khan$ytrain), Khan$xtrain)

svm_cv <- tune("svm", y ~ ., data = datos_train, kernel = 'linear',
               ranges = list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1)))

ggplot(data = svm_cv$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()
svm_cv$best.parameters

#
modelo_svm <- svm_cv$best.model

# Aciertos del modelo con los datos de entrenamiento
paste("Error de entrenamiento:", 100*mean(datos_train$y != modelo_svm$fitted), "%")
#
table(prediccion = modelo_svm$fitted, clase_real = datos_train$y)

#
datos_test <- data.frame(y = as.factor(Khan$ytest), Khan$xtest)
predicciones <- predict(object = modelo_svm, newdata = datos_test)

paste("Error de test:", 100 * mean(datos_test$y != predicciones), "%")
#
table(prediccion = predicciones, clase_real = datos_test$y)

# Implementación del dot product (geométrico)
dot_product_gemometrico <- function(x, y, alpha){
  modulo_x <- sqrt(sum(x^2))
  modulo_y <- sqrt(sum(y^2))
  aplha_radianes <- (alpha * pi) / 180
  return(modulo_x * modulo_y * cos(aplha_radianes))
}

dot_product_gemometrico(x = c(3, 5), y = c(8, 2), alpha = 45)

# Implementación del dot product (algebra)
dot_product_algebra <- function(x, y) {
  resultado <- 0
  if (length(x) != length(y)) {
    stop("La longitud de los dos vectores debe ser la misma")
  }
  for (i in seq_along(x)) {
    resultado <- resultado + x[i] * y[i]
  }
  return(resultado)
}

dot_product_algebra(x = c(3, 5), y = c(8, 2))
#PERCEPTRON
perceptron <-  function(X, y, random_seed = 553){
  # Esta función implementa el algoritmo del perceptron para encontrar un
  # hiperplano que separe correctamente las dos clases.
  
  # Transformar X en vectores aumentados
  X <- cbind(1, X)
  # Inicialización aleatoria del hiperplano
  set.seed(random_seed)
  w <- c(runif(n = 3, min = 0, max = 1))
  # Clasificación
  clasificaciones <- predict_clase(X = X, w = w)
  # Índice de las observaciones mal clasificadas
  errores_clasificacion <- which((clasificaciones != y) == FALSE)
  
  while (length(errores_clasificacion) > 0) {
    # Se selecciona aleatoriamente una observación errónea
    i <- sample(x = errores_clasificacion, size = 1)
    # Actualización del hiperplano
    w <- w + X[i,] * y[i]
    clasificaciones <- predict_clase(X = X, w = w)
    errores_clasificacion <- which((clasificaciones == y) == FALSE)
  }
  return(w)
}

predict_clase <- function(X, w){
  # Esta función devuelve la clasificación de las observaciones
  # acorde al valor de sus predictores X y al hiperplano w
  clase_predicha <- apply(X = X, MARGIN = 1, FUN = function(x){crossprod(x,w)})
  clase_predicha <- sign(clase_predicha)
  return(clase_predicha)
}

# Ejemplo observaciones linealmente separables en 2 dimensiones
X <- matrix(c(8, 4, 9, 7, 9, 4, 10, 2, 8, 7, 4, 4, 1, 2, 7, 10, 7, 10, 6, 8, 10,
              7, 3, 5, 4, 6, 3, 5), ncol = 2, byrow = FALSE)
y <- c(1, 1, 1, 1, 1, 1, 1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 )

hiperplano <- perceptron(X = X, y = y)
hiperplano

library(ggplot2)
datos <- data.frame(X, y)
ggplot(data = datos, aes(x = X1, y = X2, color = as.factor(y))) +
  geom_point() +
  # La pendiente e intersección de la recta se obtienen siguiendo los pasos
  # descritos anteriormente para obtener una recta a partir dos vectores
  geom_abline(intercept = -(hiperplano[1]/hiperplano[3]),
              slope =     -(hiperplano[2]/hiperplano[3])) +
  theme_bw() +
  theme(legend.position = "none")