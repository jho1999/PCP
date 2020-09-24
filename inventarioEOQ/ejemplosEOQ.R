source("ecuacionesEOQ.R")

#--------------ejemplo 1--------------
lambda1 <- 1300#unidades/yr
stdDev1 <-150 

orderingCost1 <-8 #$
unitCost1 <- 0.75#$

i1 <- 0.3 #%/yr

h1=i1*unitCost1 #holding cost

Qopt1 <-eoqModel(K=orderingCost1, l=lambda1, h=h1)
print(paste("The optimum cost is:", Qopt1))

Q1lower=250#unidades
Q1upper=2*250#unidades

ratio1_1 <-senAnalysis(Qopt = Qopt, Q=Q1lower)
ratio1_2 <-senAnalysis(Qopt = Qopt, Q=Q1upper)

#--------------ejemplo 2--------------
lambda2 <- 1200#units
unitCost2 <-28#$/unit

i2<-0.3#%/yr
K2<-20#$

Q2<- 100#units
h2<-i2*unitCost2 #$/unit*yr

currentCost2<-averageTotalCost(K=K2, l=lambda2, Q=Q2, c=unitCost2, h=h2)
print(paste("The actual cost is:", currentCost2))

#Grafica
costo<-c()
for (i in 1:2000) {
  costo[i]<-averageTotalCost(K=K2, l=lambda2, Q=i, c=unitCost2, h=h2)
}
plot(costo, main='Gráfica de la relación entre unidades y costo para el ejemplo 2', xlab = "Unidades", ylab='Costo')

invCurrentCost2 <-averageTotalCost(K=K2, l=lambda2, Q=Q2, h=h2)
print(paste("The actual inventory is:", invCurrentCost2))

Qopt2 <- eoqModel(K=K2, l=lambda2,h=h2)
print(paste("Q opt:", Qopt2))

optCost2<-averageTotalCost(K=K2, l=lambda2, Q=Qopt2, c=unitCost2, h=h2)
print(paste("The opt cost is:", optCost2))

optCurrentCost2 <-averageTotalCost(K=K2, l=lambda2, Q=Qopt2, h=h2)
print(paste("The opt inventory is:", optCurrentCost2))




