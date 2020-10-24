source('StaticLotSizing')

#Example 1

lambda1 <- 400#palets/month
unitCost1 <- 2500#$/pallet
i1 <- 0.145/12#% mensual
K1 <- 30#$
P1 <- 23*20#pallets/month

h1= i1*unitCost1

#Assumptions: P>1 and b = 0

Qoptimum <- Qopt(K = K1, l = lambda1, h = h1, P = P1, pi = 0, piHat = 0)
print(paste("Q* = ", Qoptimum))

H <- maxInventory(Q= Qoptimum, l = lambda1, P=P1, b = 0)
print(paste("H = ", H))

avgI <- averageInventory(Q = Qoptimum, l = lambda1, P = P1, b = 0)
print(paste("AvgI = ", avgI))

tiempos <- repDepTimes(b = 0, P = P1, l = lambda1, H = H)
print(paste("Rep tiem = ", (tiempos[1]+ tiempos[2])*20))


#Example 2
lambda2 <- 400#lb/month
P2 <- 2000#lb/month
K2 <- 120#$
unitCosst2 <- 3#$/lb
i2 <- 0.2/12#$/yr
pi2 <- 0.1#$/lb
piHat2 <- 1.2#$/lb*month

h2 <- i2*unitCosst2

Qoptimum2 <- Qopt(K = K2, l = lambda2, h = h2, P = P2, pi = pi2, piHat = piHat2)
print(paste("Q* = ", Qoptimum2))

b2 <- bOpt(h = h2, Q = Qoptimum2, l = lambda2, pi = pi2, P = P2, piHat = piHat2)
print(paste("b* = ", b2))

avgI2 <- averageInventory(Q = Qoptimum2, l = lambda2, P = P2, b = b2)
print(paste("AvgI = ", avgI2))

H2 <- maxInventory(Q= Qoptimum2, l = lambda2, P=P2, b = b2)
print(paste("H = ", H2))

tiempos2 <- repDepTimes(b = b2, P = P2, l = lambda2, H = H2)

print(paste("Tiempo para generar Inv max (H) = ", tiempos2[2]*20))
print(paste("Tiempo para generar faltante (b) = ", tiempos2[4]*20))
print(paste("Tiempo para generar recuperarse (b) = ", tiempos2[1]*20))


avgB <- averageBackorders(Q = Qoptimum2, l = lambda2, P = P2, b = b2)
montlyCost <- costFunction(c = unitCosst2, l = lambda2, K = K2, Q = Qoptimum2, h = h2, I = avgI2, 
             pi = pi2, b = b2, piHat = piHat2, B = avgB)

print(paste("costo mensual = ", montlyCost))
print(paste("costo anual = ", montlyCost*12))

#Example 3
lambda3 <- 280
unitCost3 = 2.4
K3 = 45
i3 = 0.2

Qopt3 <- Qopt(K=K3, l = lambda3, h = i3*unitCost3, P = 10000, pi = 0, piHat = 0) #P muy grande para que sea "infinito" usas valor mucho mas grande que lambda
print(paste("Q* = ", Qopt3))
