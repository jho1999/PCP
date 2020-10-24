
#Vamos a hacer funciones para llamarlas después

averageAnnualCost <- function( K = 1, l =1, Q = 1, c = 0, h = 1) {
  
  cost <- K*l/Q + c*l + 0.5*h*Q
  return(cost)
}

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