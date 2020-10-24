#aqui puse las funciones pa no usar source. 
H <- function(Q=1,lambda=1,P=Inf,b=0){
  return(Q*(1-lambda/P)-b)
}
I <- function(Q=1,P=Inf,lambda=1,b=0){
  auxH=H(Q=Q,lambda=lambda,P=P,b=b)
  return((auxH^2)/(2*(auxH+b)))
}
B <- function(Q=1,P=Inf,lambda=1,b=0){
  auxH=H(Q=Q,lambda=lambda,P=P,b=b)
  return((b^2)/(2*(auxH+b)))
}
totalCost <- function(c=0,K=0,h=0,pi=0,piHat=0,lambda=1,P=Inf,Q=0,b=0){
  auxB=B(Q = Q,P = P,lambda = lambda,b = b)
  auxI=I(Q = Q,P = P,lambda = lambda,b = b)
  invT=lambda/Q
  return(c*lambda+K*invT+h*auxI+pi*b*invT+piHat*auxB)
}
optimalb <- function(Q=0,P=Inf,lambda=1,h=1,pi=1,piHat=1){
  return((h*Q-pi*lambda)*(1-lambda/P)/(h+piHat))
}
optimalQ <- function(K=0,h=0,pi=0,piHat=0,lambda=0,P=Inf){
  optQ=2*K*lambda/(h*(1-lambda/P))-(pi*lambda)^2/(h*(h+piHat))
  if(piHat!=0)
    optQ=optQ*(h+piHat)/piHat
  return(sqrt(optQ))
}

#analisis de unidades en mi cuaderno
#literal hay que reemplazar las unidades. 
lambda1=250000 

P1=1100*250 
k1=200 
i1=.02*12 
c1=100
pi1=.4 
piHat1=10*12  

h1=i1*c1

qOpt1=optimalQ(K=k1,h = h1,pi = pi1,piHat = piHat1,lambda = lambda1,P = P1)
bOpt=optimalb(Q = qOpt1,P = P1,lambda = lambda1,h = h1,pi = pi1,piHat = piHat1)
hOpt=H(Q = qOpt1,lambda = lambda1,P = P1,b = bOpt)
Iopt=I(Q = qOpt1,P = P1,lambda = lambda1,b = bOpt)
avgB=B(Q = qOpt1,P = P1,lambda = lambda1,b = bOpt)

costoAnual=totalCost(c = c1,
                     K = k1,
                     h = h1,
                     pi = pi1,
                     piHat = piHat1,
                     lambda = lambda1,
                     P = P1,
                     Q = qOpt1,
                     b = bOpt)

#para ver con la nueva prod que onda
P2<-P1*1.195 #me movi en el rango 
P2/250
qOpt2=optimalQ(K=k1,h = h1,pi = pi1,piHat = piHat1,lambda = lambda1,P = P2)
bOpt2=optimalb(Q = qOpt1,P = P2,lambda = lambda1,h = h1,pi = pi1,piHat = piHat1)
hOpt2=H(Q = qOpt1,lambda = lambda1,P = P2,b = bOpt)
Iopt2=I(Q = qOpt1,P = P2,lambda = lambda1,b = bOpt)
avgB2=B(Q = qOpt1,P = P2,lambda = lambda1,b = bOpt)

costoAnual2=totalCost(c = c1,
                     K = k1,
                     h = h1,
                     pi = pi1,
                     piHat = piHat1,
                     lambda = lambda1,
                     P = P2,
                     Q = qOpt2,
                     b = bOpt2)

#para la ultima preg
lambda3=250000 

P3=Inf
k3=600 
i3=.02*12 
c3=120 
pi3=0
piHat3=0 
h3<-i3*c3
b3<-0

qOpt3=optimalQ(K=k1,h = h1,pi = pi1,piHat = piHat1,lambda = lambda1,P = P2)
bOpt3=optimalb(Q = qOpt3,P = P2,lambda = lambda1,h = h1,pi = pi1,piHat = piHat1)
hOpt3=H(Q = qOpt1,lambda = lambda1,P = P2,b = bOpt)
Iopt3=I(Q = qOpt1,P = P2,lambda = lambda1,b = bOpt)
avgB3=B(Q = qOpt1,P = P2,lambda = lambda1,b = bOpt)

costoAnual3=totalCost(c = c1,
                      K = k1,
                      h = h1,
                      pi = pi3,
                      piHat = piHat3,
                      lambda = lambda1,
                      P = P3,
                      Q = qOpt3,
                      b = b3)
#costo anual 3 es el mas caro de todos los costos