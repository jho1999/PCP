
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

