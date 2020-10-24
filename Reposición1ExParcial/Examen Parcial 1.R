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


####AggPP
require(lpSolve)

numberVariables = 27
numberConstraints = 21

variablesNames = c('W0','I0','B0',
                   'W1','H1','F1','I1','B1','P1',
                   'W2','H2','F2','I2','B2','P2',
                   'W3','H3','F3','I3','B3','P3',
                   'W4','H4','F4','I4','B4','P4')

constrainstNames = c('InitW', 'InitIn', 'InitBk',
                     'Wo1','Wo2', 'Wo3','Wo4',
                     'In1', 'In2','In3','In4',
                     'Pr1','Pr2','Pr3','Pr4',
                     'Bk1z','Bk2z','Bk3z','Bk4z',
                     'Inv4','W1W3')

dem <- c(632,690,858,772)

unitsPerWorkerperMonth = sum(window(tsR, start = c(2019,10)))/ ((254*10))

cW1 = 6000 #per month
cW2 = 6000
cW3 = 6000
cW4 = 6000

InitialNumberOfWorkers = 10
InitialInventory=0
InitialBackorders= 50

#Definir aquÃ� los costos
cH= 300 
cF= 600
cI= 50
cB = 75
cP = 500


objFunction <- c(0,0,0,
                 cW1,cH,cF,cI,cB,cP,
                 cW2,cH,cF,cI,cB,cP, 
                 cW3 ,cH,cF,cI,cB,cP, 
                 cW4,cH,cF,cI,cB,cP) 

A <- matrix(0, nrow = numberConstraints, ncol = numberVariables, 
            dimnames = list(constrainstNames, variablesNames))

A['InitW', 'W0'] = 1
A['InitIn', 'I0'] = 1
A['InitBk', 'B0'] = 1

intialSigns <- rep("=", 3)

initialsRHS <- c(InitialNumberOfWorkers,
                 InitialInventory,
                 InitialBackorders)

A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
A['Wo4', 'W4'] = 1; A['Wo4', 'W3'] = -1; A['Wo4', 'H4'] = -1; A['Wo4', 'F4'] = 1;

workersSigns <- rep("=", 4)

workersRHS <- rep(0,4)

A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1;
A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1;
A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1;
A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1;
inventorySigns <- rep("=", 4)

inventoryRHS <- c(-1*dem)

A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -4*24; 
A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -4*26; 
A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -4*24; 
A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -4*20; 

productionSigns <- rep("<=", 4)
productionRHS <- rep(0,4)

A['Bk1z', 'B1'] = 1
A['Bk2z', 'B2'] = 1
A['Bk3z', 'B3'] = 1
A['Bk4z', 'B4'] = 1

invBackSigns <- rep("=", 4)
invBackRHS <- rep(0,4)

#restricciÃ³n punto 2:
A['Inv4','I4'] = 1

inv4Signs <- c("=")
inv4RHS <- c(0)

#retricciÃ³n 4

A['W1W3','W1']= 2 ; A['W1W3', 'W3'] = -1

W1W3Signs <- c("<=")
W1W3RHS <- c(0)

# Find the optimal solution
aggregateProductionPlan <-  lp(direction = "min",
                               objective.in = objFunction,
                               const.mat = A,
                               const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns, invBackSigns,inv4Signs,W1W3Signs),
                               const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, invBackRHS, inv4RHS , W1W3RHS ),
                               int.vec = c(4,10,16,22) #integers Wi
)  
print(paste("The total cost is: ", aggregateProductionPlan$objval))
best_sol <- aggregateProductionPlan$solution
names(best_sol) <- variablesNames
print(best_sol)


#####StatLotSi
source('StaticLotSizingEq.R')
lambda1 <- 580#lb/month
P1 <- 5*6*20#lb/month
K1 <- (100+55)*1.5*2#$
unitCost1 <- 2.5#$/lb
i1 <- .42/12#$/month
pi1 <- 1#$/lb
piHat1 = 350

h1 <- i1*unitCost1

Qoptimum1 <- Qopt(K = K1, l = lambda1, h = h1, P = P1, pi = pi1, piHat = piHat1)
print(paste("Q* = ", Qoptimum1))

bOp1 <- bOpt(h = h1, Q = Qoptimum1, l = lambda1, pi = pi1, P = P1, piHat = piHat1)
print(paste("b* = ", bOp1))

avInv1 <- averageInventory(Q = Qoptimum1, l = lambda1, P = P1, b = bOp1)
print(paste("avgInv = ", avInv1))

avBck1 <- averageBackorders(Q = Qoptimum1, l = lambda1, P = P1, b = bOp1)
print(paste("avgbackorders = ", avBck1))

costProd <- costFunction(c = unitCost1, l = lambda1, K = K1, Q = Qoptimum1, h = h1, I = avInv1, pi = pi1, b = bOp1,
                         piHat = piHat1, B = avBck1)
print(paste("cost prod = ", costProd*12))

inv <- maxInventory(Q = Qoptimum1, l = lambda1, P = P1, b = bOp1)
print(paste("inventario = ", inv))


