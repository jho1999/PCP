#Respuestas para las preguntas 1-3

#------ DECLARACIONES -----#
#funciones auxiliares
optQ=function(K=0,lambda=0,h=0,pi=0,nR=0){
  return(sqrt(2*lambda*(K+pi*nR)/h))
}
optR=function(Q=0,lambda=0,h=0,pi=0){
  return(invF(1-Q*h/(pi*lambda)))
}
nRf=function(R=0){
  fnR=function(x=0){(x-R)*fx(x)}
  integ = integrate(f = fnR,lower = R,upper = Inf)
  return(integ$value)
}

#DATOS
lambda <- 100
c <- 10#$/unidad
K <- 10#$ #holding cost
tau <- 1.5#meses
i <- .1/12 #%/mes
pi0 <- 10#$/unidades 
h <- i*c

#Declaramos las funciones para facilitarnos la vida
fx = function(x=0){
  return(1/100*exp(-x/100))
}
invF = function(u=0){
  return(-100*log(1-u))
}

#----- Codigo -----#
j=0
Q=optQ(K = K,lambda = lambda,h = h,pi = pi0) #Es una Q inicial
R=optR(Q = Q,lambda = lambda,h = h,pi = pi0) #Es una R inicial

#numero de iteraciones a realizar
n=10

for(j in 1:n){
  nr = nRf(R = R)
  Q=optQ(K = K,lambda = lambda,h = h,pi = pi0,nR = nr)
  R=optR(Q = Q,lambda = lambda,h = h,pi = pi0)
}



beta <- 1 - nRf(R=R)/Q


#Respuestas

paste0("Pregunta 2_1: ", ceiling(R))
paste0("Pregunta 2_2: ", ceiling(Q))
paste0("Pregunta 2_3: ", ceiling(nRf(R=R)))
paste0("Pregunta 3: ",beta) #fulfill rate
