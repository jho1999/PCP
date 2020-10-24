
#dices donde est√°n las funciones que vas a utilizar

source('ecuacionesEOQ.R')

#Example 1: diapositiva 18/25

lambda1 <- 1300 #units/year
stdDev1 <- 150

orderingCost1 <- 8 #$
unitCost1 <- 0.75 #$

i1 <- .3 #%/year

h1 <- i1*unitCost1

Qopt <- eoqModel ( K = orderingCost1, l = lambda1, h = h1)
print(paste('El eoq √≥ptimo =', Qopt))

Q1lower <- 250 #unidades
Q1upper <- 2*250 #unidades

ratio1 <- senAnalysis(Qopt = Qopt, Q = Q1lower)
ratio2 <- senAnalysis(Qopt = Qopt, Q = Q1upper)

#-------------
#Example 3  (diap 24/25)
#-------------

lambda2 <- 1200#units/yr
unitCost2 <- 28#$/unit

i2 <- 0.3#%/yr
K2 <- 20#$

h2 <- i2*unitCost2

Q2 <- 100#units

currentCost <- averageAnnualCost(K = K2, l = lambda2, Q = Q2, c = unitCost2, h = h2)
print(paste('The actual cost is', currentCost))

invCurrentCost <- averageAnnualCost( K = K2, l = lambda2, Q = Q2, h = h2)
print(paste('The actual inventory cost is', invCurrentCost))

#2)Qopt

Qopt2 <- eoqModel(K=K2, l = lambda2, h = h2)
print(paste("Qopt = ", Qopt2))

costOpt <- averageAnnualCost(K = K2, l = lambda2, Q = Qopt2, c = unitCost2, h = h2)
print(paste('The actual cost is', costOpt))

invOptCost <- averageAnnualCost( K = K2, l = lambda2, Q = Qopt2, h = h2)
print(paste('The actual inventory cost is', invOptCost))

#3) N˙mero de Ûrdenes
NoOrdersYr <- lambda2/Qopt2
print(paste("No orders per year =", NoOrdersYr))


#-----------
#Example 3 
#-----------

lambda3 <- 4 #units/day
stdDev3 <- 3

tau <- 4#units

#1)
lambdaTau <- lambda3*tau #Es el punto de reorden
stdDevTau <- stdDev3*sqrt(tau)

#2)
serviceLevel1 <- .95
reorderPoint <- reorderPointNormal(lt = lambdaTau, alpha = serviceLevel1, sigmaTau = stdDevTau)
print(paste("R = ", reorderPoint))

#3) Safety stock
safetyStock <- reorderPointNormal(lt = 0, alpha = serviceLevel1, sigmaTau = stdDevTau)
print(paste("Service level =", serviceLevel1, "R =", reorderPoint, "safety stock = ", safetyStock))

holdingCost <- safetyStock*h2
