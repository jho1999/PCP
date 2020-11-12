#funciones auxiliares
optQ=function(K=0,lambda=0,h=0,p=0,nR=0){
  return(sqrt(2*lambda*(K+p*nR)/h))
}
optR=function(Q=0,lambda=0,h=0,p=0){
  return(qnorm(1-((Q*h)/(p*lambda)),lambdatau,sigmatau))
}
lostFunctionNormal=function(x){exp(-(x^2)/2)/(sqrt(2*pi))-x*(1-pnorm(x))}
nRf=function(R=0){
  z=(R-lambdatau)/sigmatau
  return(lostFunctionNormal(z)*sigmatau)
}

# Problemas 7-10 del quiz 5
#datos
lambda1 = 10000
K1 = 200
i = .2 
c = 5
h1= i*c
pi1=4

#esto lo sabemos por que es normal
lambdatau=400 
sigmatau=sqrt(900)

Q0 = optQ(K = K1, lambda = lambda1, h = h1, p = pi1, nR = nRf(R=400))


ciclos<-(lambda1/Q0)
shortCost <- pi1*nRf(R=400)*ciclos

lambdaNotMet <- (nRf(R=400)/Q0)*100


#Respuestas
paste0("Pregunta 8: ", Q0) #Q optima
paste0("Pregunta 9: ", shortCost) #annual shortage cost
paste0("Pregunta 10: ", lambdaNotMet)
