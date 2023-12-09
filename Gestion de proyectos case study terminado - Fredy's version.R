# Paso 1: Cargando las bibliotecas necesarias
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(readxl)

# Paso 2: Cargando los datos desde el archivo Excel
customers_data <- AWcustomersandsales

# Paso 3: Selección de variables relevantes para el análisis
selected_data <- customers_data %>%
  select(Gender, MaritalStatus, HomeOwnerFlag, NumberCarsOwned, NumberChildrenAtHome, TotalChildren, BikeBuyer, AvgMonthSpend)

# Convertir variables a los tipos correctos
selected_data$Gender <- as.factor(selected_data$Gender)
selected_data$MaritalStatus <- as.factor(selected_data$MaritalStatus)
selected_data$AvgMonthSpend <- as.numeric(selected_data$AvgMonthSpend)

# Paso 4: Limpieza de datos - Filtrando filas donde 'BikeBuyer' es nulo
selected_data <- selected_data %>%
  filter(!is.na(BikeBuyer))

# Paso 5: División del conjunto de datos en entrenamiento y validación (80/20)
set.seed(123)  # Para reproducibilidad
training_index <- createDataPartition(selected_data$BikeBuyer, p = 0.8, list = FALSE)
training_set <- selected_data[training_index, ]
validation_set <- selected_data[-training_index, ]

# Comparación de las proporciones de 'BikeBuyer' en los conjuntos de entrenamiento y validación
print("Proporciones en el conjunto de entrenamiento:")
print(prop.table(table(training_set$BikeBuyer)))

print("Proporciones en el conjunto de validación:")
print(prop.table(table(validation_set$BikeBuyer)))

# Paso 6: Entrenamiento del modelo de árbol de decisión
tree_model <- rpart(BikeBuyer ~ ., data = training_set, method = 'class')

# Paso 7: Visualización del árbol de decisión
rpart.plot(tree_model, cex = 0.6)

# Paso 8: Realización de predicciones en el conjunto de validación
predictions <- predict(tree_model, validation_set, type = 'class')

# Paso 9: Conversión de predicciones y valores reales a factores con los mismos niveles
predictions_factor <- factor(predictions, levels = levels(training_set$BikeBuyer))
BikeBuyer_factor <- factor(validation_set$BikeBuyer, levels = levels(training_set$BikeBuyer))

# Comprobando si hay niveles en común antes de la matriz de confusión
if(length(intersect(levels(predictions_factor), levels(BikeBuyer_factor))) > 0) {
  confusionMatrix(predictions_factor, BikeBuyer_factor)
} else {
  warning("No hay niveles en común entre las predicciones y los valores reales.")
}

# Paso 10: Mostrando la matriz de confusión

confusion_table <- table(predictions, validation_set$BikeBuyer)
print(confusion_table)

# Paso 10.1: Evaluación adicional del modelo
confusionMatrix(predictions, as.factor(validation_set$BikeBuyer))


# Paso 11: Realizando predicciones específicas
# Asegurándose de que los niveles de los factores coincidan con los del modelo entrenado
levels_gender <- levels(training_set$Gender)
levels_marital_status <- levels(training_set$MaritalStatus)

prediction_data_1 <- data.frame(Gender = factor('Male', levels = levels_gender), 
                                MaritalStatus = factor('Single', levels = levels_marital_status), 
                                HomeOwnerFlag = 0, NumberCarsOwned = 0, 
                                NumberChildrenAtHome = 0, TotalChildren = 0, 
                                AvgMonthSpend = 50)

prediction_data_2 <- data.frame(Gender = factor('Male', levels = levels_gender), 
                                MaritalStatus = factor('Married', levels = levels_marital_status), 
                                HomeOwnerFlag = 1, NumberCarsOwned = 3, 
                                NumberChildrenAtHome = 3, TotalChildren = 3, 
                                AvgMonthSpend = 10)

prediction_1 <- predict(tree_model, newdata = prediction_data_1, type = 'class')
prediction_2 <- predict(tree_model, newdata = prediction_data_2, type = 'class')

# Imprimiendo las predicciones específicas
print(prediction_1)
print(prediction_2)
