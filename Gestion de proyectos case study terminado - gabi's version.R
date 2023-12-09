library(readxl)
install.packages("rpart")
install.packages('caTools')

#Liberia nueva para calcular la precisión de la matriz de confusión
install.packages("caret")
library(caret)

datos <- dataset
library(dplyr)

datos = select(datos,BikeBuyer,CountryRegionName,Education,Occupation,Gender,MaritalStatus,HOMEOWNER,NumberChildrenAtHome,AvgMonthSpend,YearlyIncome,NumberCarsOwned,CountryRegionName)
datos = na.omit(datos)

library(rpart)
library(rpart.plot)

######
# vamos a dividir la muestra en set de entrenamiento y validacion
######

library('caTools')
library(dplyr)
datos $HOMEOWNER <- as.factor(datos$HOMEOWNER) 
#City,StateProvinceName,CountryRegionName,Education,Occupation,Gender,MaritalStatus,NumberCarsOwned,NumberChildrenAtHome,TotalChildren,YearlyIncome,AvgMonthSpend,HOMEOWNER
datos $CountryRegionName <- as.factor(datos$CountryRegionName)
datos $Gender <- as.factor(datos$Gender)
datos $MaritalStatus <- as.factor(datos$MaritalStatus)
datos $Occupation <- as.factor(datos$Occupation)
datos $BikeBuyer <- as.factor(datos$BikeBuyer)

division <- sample.split(datos$BikeBuyer,SplitRatio = 0.8)
#creamos training y validationage

Training <- subset(datos,division==TRUE)
Validation <- subset(datos,division==FALSE)
#Comprobamos las proporciiones 

prop.table(table(Validation$BikeBuyer))
prop.table(table(Training$BikeBuyer))

#creando el arbol
arbol_entrenamiento <- rpart(BikeBuyer~HOMEOWNER+CountryRegionName+Gender+MaritalStatus+Occupation+AvgMonthSpend+NumberChildrenAtHome+YearlyIncome+NumberCarsOwned+CountryRegionName,data = Training,method = "class")
rpart.plot(arbol_entrenamiento)

#hacemos una prediccion con este modelo pero usando la valdaction

prediction_validation <- predict(arbol_entrenamiento,newdata = Validation,type = "class")

# Vemos la matriz de confusion
table(prediction_validation,Validation$BikeBuyer)
#Comparamos preccion con datos reales
predict(arbol_entrenamiento,newdata = data.frame(NumberChildrenAtHome=1,NumberCarsOwned=2,YearlyIncome=81076,HOMEOWNER="1",CountryRegionName="Canada",Gender="M",MaritalStatus="M",Occupation="Clerical",AvgMonthSpend=53.11))

#Evaluación adicional del modelo para comprobar precisión
confusionMatrix(prediction_validation, Validation$BikeBuyer)