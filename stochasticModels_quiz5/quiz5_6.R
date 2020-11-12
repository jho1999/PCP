#Preguntas 19-22
#datos
lambda<-15
sigma<-5
tau<-5/12
t<-2/12
h<-0.2*500
pi0<-5000

lambdatau<-lambda*(tau+t)
sigmatau=sigma*sqrt(tau+t)
Q<-lambdatau

alfa<-1-Q*h/(pi0*lambda)

R<-lambdatau +qnorm(alfa)*sigmatau

S<-R

lostFunctionNormal=function(x){exp(-(x^2)/2)/(sqrt(2*pi))-x*(1-pnorm(x))}
beta <- 1 - lostFunctionNormal(qnorm(alfa))*sigmatau/lambdatau


ss<-R-lambdatau
#Respuestas
paste0("Pregunta 20: ", S)
paste0("Pregunta 21_1: ",ceiling(alfa*100))
paste0("Pregunta 21_2: ",ceiling(beta*100))
paste0("Pregunta 22: ",ss )
