# Nombre:
# Fecha:

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
# Datos
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine
/wine.data", "wine.data")
# Informaci�n
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine
/wine.names", "wine.names")

readLines("wine.data", n = 10)
vino <- read.table("wine.data", sep = ",", header = FALSE)
vino
# veamos los indicadores generales
summary(vino)
# Copiamos los nombres y los enviamos a un archivo txt

file.copy(from = "wine.names", to = "wine_names.txt")
#Mostramos el archivo
file.show("wine_names.txt")

nombres <-
  readLines("wine_names.txt")[58:70] %>%
  gsub("[[:cntrl:]].*\\)", "", .) %>%
  trimws() %>%
  tolower() %>%
  gsub(" |/", "_", .) %>%
  # Agregamos el nombre "tipo", para nuestra primera columna con los tipos de
  #vino
c("tipo", .)
# Agregamos las cabeceras al achivo de vinos
names(vino) <-nombres
vino <- vino %>%
  mutate_at("tipo", factor)
vino
# Definimos la semilla en 1649
set.seed(1649)
# El % del set de entrenamiento 70%
vino_entrenamiento <- sample_frac(vino, .7)
# asignamos el 30% restante a pruebas
vino_prueba <- setdiff(vino, vino_entrenamiento)

arbol_1 <- rpart(formula = tipo ~ ., data = vino_entrenamiento)
arbol_1
#gRAFICAMOS
rpart.plot(arbol_1)
#cALCULAMOS LA PREDICCION
prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")
#gENERAMOS LA MATRIZ DE CONFUSION
confusionMatrix(prediccion_1, vino_prueba[["tipo"]])
