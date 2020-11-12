
WW<-function(lambda, h, c=rep(0,length(lambda)), K){
  #inicio de la funcion
  n=length(lambda) #numero de periodos a considerar
  minAnt=0
  min=0
  CM=matrix(data = 0, nrow = n, ncol = n)

  
  hc=matrix(data = 0, nrow = n, ncol = n)
  for(ren in 1:(n-1)){
    for(col in (ren+1):n){
      hc[ren,col]=hc[ren,(col-1)]+sum(h[ren:(col-1)])*lambda[col]
    }

  }

  for(i in 1:n){  #para recorrer los renglones
    costI<-minAnt+K[i]
    ci<-c[i]
    for(j in i:n){ #para recorrer las columnas
      CM[i,j]=costI+ci*sum(lambda[i:j])+hc[i,j]   #guardamos el costo en la matriz
    }
    minAnt=min(CM[1:i,i])  #guardamos el minimo de la columna anterior
  }

  #analisis
  i=n
  Q=rep(0,n)
  auxString<-""
  while(i>0){
    minI=which.min(CM[1:i,i])
    auxString<-paste(auxString,paste("Se pide en ",minI," para ",minI," a ",i),"\n")
    Q[minI]=sum(lambda[minI:i])
    i=minI-1
  }
  
  #para llenar los vectores necesarios para la ultima matriz
  l<-rep(0,n)
  for (k in 1:n) {
    if(Q[k]>0){
      l[k]=Q[k]-lambda[k]
    }else{
      l[k]=l[k-1]-lambda[k]
      K[k]=0
    }
  }
  h<-h*l
  cQ<-Q*c
  costo<-cQ+K+h
  costo
  
  matAnalysis<-rbind(lambda,Q,c,l,cQ,K,h,costo)
  
  #resultados
  cat(auxString)
  WW<-list("cost"=minAnt, "costMatrix"=CM, "analysis"=matAnalysis)
  return(WW)
}


lambda = c(60,100,10,200,120,15)
h = c(0.8,0.8,0.8,1.8,2,2)
c = c(5,5,5,5,5,5)
K = c(150,110,120,200,200,200)

WW1<-WW(lambda = lambda, c=c, h=h, K=K)

