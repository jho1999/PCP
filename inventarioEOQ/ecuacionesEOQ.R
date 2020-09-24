#aqui vamos a crear funciones

#le damos valores por default.
averageTotalCost <-function(K=1, l=1, Q=1, c=0, h=1){
  cost <-K*l/Q+c*l+.5*h*Q
  return(cost)
  #tarea moral: hacer una tablita con puntos
}

#cantidad optima para ordenar
eoqModel <- function(K=1, l=1, h=1){
  optQ <-sqrt(2*K*l/h)
  return(optQ)
}

senAnalysis <-function(Qopt=1, Q=1){
  #desviacion del costo minimo=ratio-Qopt
  ratio<-0.5*(Qopt/Q+Q/Qopt)
  return(ratio)
}

reorderPointNormal <-function(lt=1, alfa=1, sigmaTau=1){
  R=lt+qnorm(alfa)*sigmaTau
  return(R)
}
#z_alfa=qnorm(alfa)