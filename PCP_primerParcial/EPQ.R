#EPQ
costFunction <- function(c = 0, l = 1, K = 1, Q = 1, h = 1, I = 1, pi = 1, b = 1,
                         piHat = 1, B = 1){
  G = c*l + K*l/Q + h*I + pi*b*l/Q + piHat*B
  return(G)
}

averageInventory <- function(Q = 1, l = 1, P = 1, b = 1){
  avgInv = (Q*(1-l/P)-b)^2 / (2*Q*(1-l/P))
  return(avgInv)
}

averageBackorders <- function(Q = 1, l = 1, P = 1, b = 1){
  avgBck = b^2 / (2*Q*(1-l/P))
  return(avgBck)
}

maxInventory <- function(Q = 1, l = 1, P = 1, b = 1){
  H = Q*(1-l/P)-b
  return(H)
}

Qopt <- function(K = 1, l = 1, h = 1, P = 1, pi = 1, piHat = 1){
  
  if(pi != 0){
    Q = sqrt( (2*K*l/(h*(1-l/P)) )- ((pi*l)^2/(h*(h+piHat)) )) *sqrt((h+piHat)/piHat)
  }else{
    Q = sqrt( (2*K*l/(h*(1-l/P)) )- ((pi*l)^2/(h*(h+piHat)) ))
  }
  
  return(Q)
}

bOpt <- function(h = 1, Q = 1, l = 1, pi = 1, P = 1, piHat = 1){
  bOptimum <- ( (h*Q-pi*l) * (1-l/P) )/(h+piHat)
  return(bOptimum)
}

repDepTimes <- function( b = 1, P = 1, l = 1, H = 1){
  T1 <- b/(P-l)
  T2 <- H/(P-l)
  T3 <- H/l
  T4 <- b/l
  timeDepRep <- c(T1,T2,T3,T4)
  return(timeDepRep)
}
#Vamos a hacer funciones para llamarlas después

eoqModel <- function( K = 1, l = 1, h = 1){
  
  optQ <- sqrt(2*K*l/h)
  return(optQ)
}

senAnalysis <- function(Qopt = 1, Q = 1){
  
  ratio <- 0.5*(Qopt/Q + Q/Qopt)
  return(ratio)
}

reorderPointNormal <- function(lt = 1, alpha = 1, sigmaTau = 1){
  R = lt + qnorm(alpha)*sigmaTau
  return(R)
}

holdingCost <- function(Q = 1,  h = 1){
  hC = .5*h*Q
  return(hC)
}

orderingCost <- function( K = 1, l =1, Q = 1, c = 0, h = 1){
  oC = K*(l/Q) + c*l
  return(oC)
}


#Ejercicio
lambda1<-250000#unit year
P1<-1100#unit*daily
P1<-P1*250
K1<-200*12 #setup cost
c1=100#$ unit
pi1<-0.4 #unit
piHat1<-10*12 #por faltantes eanuales 
i1=.02*12


#1 año son 250 días y 12 meses



optQ1<-Qopt(K=K1, h=h1, pi=pi1, piHat=piHat1, l = lambda1, P=P1)
optb1<-bOpt(Q=optQ1, P=P1, l=lambda1, pi=pi1, piHat = piHat1, h=h1)
avgI<-averageInventory(Q=optQ1, l=lambda1, P=P1, b=optb1)
avgB1<-averageBackorders(Q=optQ1,l=lambda1, P=P1, b=optb1)

avgCost1<-costFunction(c=c1, l=lambda1, K=K1, Q=optQ1, h=h1, I=avgI, b=optb1, piHat=piHat1, B=avgB1)

#Para comprar:
lambda2<-lambda1
P2<-Inf
K2<-600
c2<-2.5#costo unitario
pi2<-0
piHat2<-0
h2<-h1
b<-0

optQ2<-Qopt(K=K2, h=h2, pi=pi2, piHat=piHat2, l = lambda2, P=P2)
#optb2<-bOpt(Q=optQ2, P=P2, l=lambda2, pi=pi2, piHat = piHat2, h=h2)
#avgI2<-averageInventory(Q=optQ2, l=lambda2, P=P2, b=optb2)
#avgB2<-averageBackorders(Q=optQ2,l=lambda2, P=P2, b=optb2)

avgCost2<-costFunction(c=c2, l=lambda2, K=K2, Q=optQ2, h=h2, I=avgI2, b=0, piHat=piHat2, B=avgB2)
anualCost2<-avgCost2*12
