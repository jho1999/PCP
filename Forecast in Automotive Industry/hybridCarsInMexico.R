#Carga de paquetes
library(readxl)
library(fpp2)
library(ggplot2)

#-----------------------
#---------Carga de datos
#-----------------------

#Lee los datos del archivo de excel, saltando las primeras 3 filas pues estaban sucias:
hybridCarsMX <- read_excel("RAIAVL_11.xlsx", skip = 3, sheet = "RAIAVL_11", col_names = FALSE)

#Hacemos modificcaciones a los datos para poder tratarlos adecuadamente
#Cambiamos los valores '-' por 0 para poder hacer operaciones

for (i in 1:1782) {
  if(hybridCarsMX[i,4] == '-'){
    hybridCarsMX[i,4] <- '0'
  }
}

#Modificamos los datos para tomar el agregado cada mes
for (i in 0:53) { #1749=53*33
  suma<-0
  aux<-i*33
  for (j in 1:33) {
    #aux<-aux+j
    suma<-suma+as.double(hybridCarsMX[aux+j,4])
  }
  if (i==0){
    hybridCarsMXData<-c(suma)
  }else{
    hybridCarsMXData <- c(hybridCarsMXData, suma)
  }
}

#----Hacemos serie de tiempo
tsHybridCarsMX <- ts(hybridCarsMXData, start = c(2016,1), frequency = 12 )

#--------------------------------------------------
#Análisis de la demanda de autos híbridos en México
#--------------------------------------------------

#-------Gráfica la demanda de autos híbridos:
autoplot(tsHybridCarsMX) + geom_point(color = "red", size =1 ) + ylab("Hybrid cars in México") + 
  xlab("Time")  + theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major = element_line(colour = "grey20", linetype = "dashed", size = 0.1)) +
  scale_x_continuous(breaks = scales::extended_breaks(5)) 

#------- Verificar condiciones---------
ggAcf(tsHybridCarsMX, lag.max = 54)
#Las autocorrelaciones son significativamente distintas de cero, por lo que podemos hacer un análisis de la prodcucción
Box.test(tsHybridCarsMX, lag = 1, type = "Ljung")
#Como p-value = 7.507e-10 es menor a 0.5 sabemos que los datos en general no son aletaorios por lo que podemos hacer pronósticos

#------------------------------------------
#Encontrar el mejor método para pronosticar
#------------------------------------------

trainingData <- window(tsHybridCarsMX, end = c(2019, 12))
testData <- window(tsHybridCarsMX, start = c(2020,1))

#---------Hacer pronósticos-----
averageMethod <- meanf(trainingData, h=6) 
naiveMethod <- naive(trainingData, h= 6) 
seasonalNaiveMethod <- snaive(trainingData, h=6)
fcSimpleExpSmoo <- ses(trainingData, initial = c("optimal"), h= 6) 
holtForecast <- holt(trainingData, h=6)
hwAditive <- hw(trainingData, seasonal = "additive", h= 6)
hwMultiplicative <- hw(trainingData, seasonal = "multiplicative", h = 6)

#-------Checar la exactitud de los métodos
accuracy(averageMethod)
accuracy(naiveMethod)
accuracy(seasonalNaiveMethod)
accuracy(fcSimpleExpSmoo)
accuracy(holtForecast)
accuracy(hwAditive)
accuracy(hwMultiplicative)
#El método con menor RMSE es el de Holt Winters Multiplicativo

#-------Checar exactitud con respecto a los datos de prueba
accuracy(averageMethod, testData)
accuracy(naiveMethod, testData)
accuracy(seasonalNaiveMethod, testData)
accuracy(fcSimpleExpSmoo, testData)
accuracy(holtForecast, testData)
accuracy(hwAditive, testData)
accuracy(hwMultiplicative, testData)
#El método con mejor U de theil es el método de Holt Winters Multiplicativo

#Graficamos los sets de entrenamiento
autoplot(trainingData,series = "Original Demand") + ylab("Demanda") +
  #autolayer(fitted(averageMethod),series = "Average Method") + 
  #autolayer(fitted(naiveMethod),series = "Naive Method") + 
  #autolayer(fitted(seasonalNaiveMethod),series = "Seasonal Naive Method") +
  #autolayer(fitted(fcSimpleExpSmoo),series = "Exp. Smoothing") + 
  #autolayer(fitted(holtForecast),series = "Holt") + 
  autolayer(fitted(hwAditive),series = "HW Add") + 
  autolayer(fitted(hwMultiplicative),series = "HW Mult")

#----------------------------------------
#Pronosticamos con el método seleccionado (Holt Winters Multiplicativo)
#----------------------------------------

theSelectedMethod <- hw(tsHybridCarsMX, seasonal = "multiplicative", h = 6)
autoplot(tsHybridCarsMX) + xlab("Year") + ylab("Hybrid cars in Mexico") +
  autolayer(theSelectedMethod, PI=FALSE, series = "Multiplicative Holt Winters")

#----Analizamos los residuales
ggAcf(theSelectedMethod$residuals, lag.max = 53) 
#no se puede inferir la aleatoriedad pues el primer elemento es significativamente distinto de cero
Box.test(theSelectedMethod$residuals, lag = 1, type = "Ljung") 
#el p-value es de 0.00111 por lo que no podemos inferir que los residuales sean aleatorios

#debido a que no podemos inferir residuales aleatorios se prueba con el segundo mejor metodo de las pruebas
#es decir, se elige el metodo de HW aditivo, el que tiene la segunda mejor RMSE y U de Theil

#----------------------------------------
#Pronosticamos con el método seleccionado (Holt Winters Aditivo)
#----------------------------------------

theSelectedMethod <- hw(tsHybridCarsMX, seasonal = "additive", h = 6)
autoplot(tsHybridCarsMX) + xlab("Year") + ylab("Hybrid cars in Mexico") +
  autolayer(theSelectedMethod, PI=FALSE, series = "Additive Holt Winters")

#----Analizamos los residuales
ggAcf(theSelectedMethod$residuals, lag.max = 53) 
#los residulaes parecen ser aleatorios pues sus correlaciones no son significactivamente distintas de cero.
Box.test(theSelectedMethod$residuals, lag = 1, type = "Ljung") 
#el p-value es de 0.8201 por lo que podemos inferir que los errores son aleatorios.