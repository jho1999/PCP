####ejercicio 4
#funciones auxiliares
optQ=function(K=0,lambda=0,h=0,p=0,nR=0){
  return(sqrt(2*lambda*(K+p*nR)/h))
}
optR=function(Q=0,lambda=0,h=0,p=0){
  return(Finv(1-Q*h/(p*lambda)))
}
nRf=function(R=0){
  fnR=function(x=0){(x-R)*fx(x)}
  integral <- integrate(f = fnR,lower = R,upper = b) #upper es Inf, excepto en la uniforme (upper=b)
  return(integral$value)
}

#datos
lambda0=6000 #anuales
c0 = 20#$
i0 = .3 #%
K0 = 150
p0 = 50 #$
h0 = i0*c0

lambdaTau = (a+b)/2

n=6 #numero de iteraciones a realizar
#datos de la uniforme
a = 100
b = 200 

#funciones del problema
fx = function(x=0){
  return((1/(b-a))) 
}
Finv = function(u=0){
  return(u*(b-a)+a)
}


j=0
Q=optQ(K = K0,lambda = lambda0,h = h0,p = p0)
R=optR(Q = Q,lambda = lambda0,h = h0,p = p0)

for(j in 1:n){
  nr = nRf(R = R)
  Q=optQ(K = K0,lambda = lambda0,h = h0,p = p0,nR = nr)
  R=optR(Q = Q,lambda = lambda0,h = h0,p = p0)
}
#psafety stock
s = R - lambdaTau
#costo
cost <- h0*(R-lambdaTau + Q/2) + K0*(lambda0/Q) + p0*nr*(lambda0/Q)

#Respuestas
#redondear la 12
paste0("Pregunta 12_1: ", ceiling(R))
paste0("Pregunta 12_2: ", ceiling(Q)) 
paste0("Pregunta 13: ", s)
paste0("Pregunta 14: ", cost)
