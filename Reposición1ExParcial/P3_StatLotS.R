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


