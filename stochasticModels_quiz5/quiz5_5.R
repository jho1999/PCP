#funciones auxiliares
optQ=function(K=0,lambda=0,h=0,p=0,nR=0){
  return(sqrt(2*lambda*(K+p*nR)/h))
}
optR=function(Q=0,lambda=0,h=0,p=0){
  return(qnorm(1-((Q*h)/(p*lambda)),lambdatau,sigmatau))
}
lostFunctionNormal=function(x){exp(-(x^2)/2)/(sqrt(2*p))-x*(1-pnorm(x))}
nRf=function(R=0){
  z=(R-lambdatau)/sigmatau
  return(lostFunctionNormal(z)*sigmatau)
}

#datos
lambda0=500
tau0= 2 #semanas
K0=300
c0 = 40
i0 = .2/52 #hay que volverlo semanal
h0=c0 * i0
p0 = 2 #$
sigma0 = sqrt(2500)

lambdatau=lambda0*tau0
sigmatau=sigma0*sqrt(tau0)

#No stockouts -> nR = 0
nr <- 0



Q = optQ(K = K0, lambda = lambda0, h = h0, p = p0, nR = nr)
R = optR(Q = Q, lambda = lambda0, h = h0, p = p0)

#service level
serviceLevel<-pnorm(R, lambdatau, sigmatau, TRUE) #Lo multiplico por el formato.


#fulfill rate
fulfillRate <- 1 - nr/Q


#inventario promedio
inv <-(R - lambdatau + Q/2)


#Respuestas
paste0("Pregunta 16: ", serviceLevel*100)
paste0("Pregunta 17: ", fulfillRate*100)
paste0("Pregunta 18: ", inv)
