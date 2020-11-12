# WW algorithm
# by SLGA

# Function declaration and parameters -------------------------------------
WW=function(lambda = c(0,0,0,0,0,0),
            h = c(0,0,0,0,0,0),
            c = rep(0,length((lambda))),
            K = c(0,0,0,0,0,0)){
  
  # Peterson-Silver rule verification ---------------------------------------
  
  #If V<0.25 an EPQ method is recommended, given that the demand is fairly
  #constant. If V>0.25 the WW method is appropiate.
  V=length(lambda)*sum(lambda^2)/(sum(lambda))^2-1
  if(V<0.25){warning("Peterson-Silver < 0.25, EPQ is recommended in this case.")}
  
  # Cost matrix declaration -------------------------------------------------
  
  n=length(lambda)  #periods to consider
  pMin=0          #variable to store the previous column minimum
  CM=matrix(data = 0, nrow = n, ncol = n) #nxn cost matrix
  hc=matrix(data = 0, nrow = n, ncol = n) #holding costs matrix
  
  # Cost matrix construction ------------------------------------------------
  
  for(i in 1:(n-1)){  #for each row
    for(j in (i+1):n){  #for each column
      #holding costs for each cell is computed iteratively by adding the holding
      #costs due to ordering on period i for demand j-1 and the additional cost
      #of ordering also on period i for demand j
      hc[i,j]=hc[i,(j-1)]+sum(h[i:(j-1)])*lambda[j] 
    }
  }
  
  for(i in 1:n){  #for each row
    for(j in i:n){  #for each column
      #total cost for each cell is computed by adding the ordering cost, the
      #unit cost times the order size and the holding costs
      CM[i,j]=pMin+K[i]+c[i]*sum(lambda[i:j])+hc[i,j] 
    }
    pMin=min(CM[1:i,i])  #previous column minimum is stored
  }
  
  # Results -----------------------------------------------------------------

  return(CM)
}

# Test --------------------------------------------------------------------

lambda = c(100,100,200,100,120,80)
h0=0.4+0.07+0.5/52*3
h = rep(h0,6)
c = c(3,3,3,3,3,3)
K = c(50,50,50,50,50,50)

res=WW(lambda = lambda,h = h,c = c,K = K)
res
