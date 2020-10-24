#Cargar paquetes
library(readxl)
library(fpp2)
library(ggplot2)

###############Pregunta 1 -8##############
# -----------------------------
## 1. Analisis de la demanda 
#------------------------------

demandR <- read_excel("demandDataExam1.xlsx", sheet = "Sheet1", col_names = TRUE)
tsR <- ts(demandR, start = c(2010,9), frequency = 12 )

autoplot(tsR) + geom_point(color = "red", size =1 ) + ylab("Demand") + 
  xlab("Month")  + theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                         panel.grid.major.y = element_blank(),
                         panel.grid.minor.y = element_blank(),
                         panel.grid.major = element_line(colour = "grey20", linetype = "dashed", size = 0.1)) +
  scale_x_continuous(breaks = scales::extended_breaks(20)) 

#Estadística
#Dos condiciones: 
ggAcf(tsR, lag.max = 120) #Sean signif. distintas de cero (las barritas se salgan de las lineas)
Box.test(tsR, lag = 1, type = "Ljung") #queremos p-value< 0.05

#--------------------------------------------------
## 2. Encontrar el mejor método para pronosticar 
#--------------------------------------------------

trainingData <- window(tsR, end = c(2019,9)) #con esto calculas el pronóstico
testData <- window(tsR, start = c(2019,10)) #Con esto puedes comparar el pronóstico

#Hacer los pron?sticos
averageMethod <- meanf(trainingData, h=12) 
naiveMethod <- naive(trainingData, h= 12) 
seasonalNaiveMethod <- snaive(trainingData, h=12)
fcSimpleExpSmoo <- ses(trainingData, initial = c("optimal"), h= 12) 
holtForecast <- holt(trainingData, h=12)
hwAditive <- hw(trainingData, seasonal = "additive", h= 12)
hwMultiplicative <- hw(trainingData, seasonal = "multiplicative", h = 12)

#Checar cual es el mejor m?todo

accuracy(averageMethod, testData)
accuracy(naiveMethod, testData)
accuracy(seasonalNaiveMethod, testData)
accuracy(fcSimpleExpSmoo, testData)
accuracy(holtForecast, testData)
accuracy(hwAditive, testData)
accuracy(hwMultiplicative, testData)

# --------------------------------
# 3.Forecast with selected methods
# --------------------------------

theSelectedMethod <-  hw(tsR, seasonal = "multiplicative", h= 12) #HAces pronostico con m?todo seleccionado

autoplot(tsR) + xlab("Year") + ylab("Demand") +
  autolayer(theSelectedMethod, PI=FALSE, series = "Método de promedios") #grafica ts + pronostico

# -------------------------
#4. Analisis de residuales
#--------------------------

ggAcf(theSelectedMethod$residuals, lag.max = 120) #queremos que no sean signif dif de cero
Box.test(theSelectedMethod$residuals, lag = 1, type = "Ljung") #queremos pvalue > 0.05
