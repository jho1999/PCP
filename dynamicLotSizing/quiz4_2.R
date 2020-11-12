

WW<-function(lambda, h, c=rep(0,length(lambda)), K){
  #inicio de la funcion
  n=length(lambda) #numero de periodos a considerar
  minAnt=0
  min=0
  CM=matrix(data = 0, nrow = n, ncol = n)
  #aux=matrix(data = 0, nrow = n, ncol = n)
  
  hc=matrix(data = 0, nrow = n, ncol = n)
  for(ren in 1:(n-1)){
    #aux[ren,ren]=min+K[ren]+c[ren]*lambda[ren]
    for(col in (ren+1):n){
      hc[ren,col]=hc[ren,(col-1)]+sum(h[ren:(col-1)])*lambda[col]
      #aux[ren,col]=hc[ren,col]+min+K[ren]+c[ren]*sum(lambda[ren:col])
    }
    #min=min(aux[1:ren:ren])
  }
  #aux[n,n]=min+K[n]+c[n]*lambda[n]
  
  for(i in 1:n){  #para recorrer los renglones
    for(j in i:n){ #para recorrer las columnas
      CM[i,j]=minAnt+K[i]+c[i]*sum(lambda[i:j])+hc[i,j]   #guardamos el costo en la matriz
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
  matAnalysis<-rbind(lambda,Q,c,l,cQ,K,h,costo)
  
  #resultados
  cat(auxString)
  WW<-list("cost"=minAnt, "costMatrix"=CM, "analysis"=matAnalysis)
  return(WW)
}

#Segundo ejercicio
lambda<- c(100,100,200,100,120,80)


k<-25*2 #se necesitan 2 trabajadores
i<-.5/52 #para que sea semanal
c<-c(rep(3,6))
h<-i*c+(.4+.07)
K<-c(rep(k,6))

WW2<-WW(lambda = lambda, c=c, h=h, K=K)
#El costo total sera de:
WW2$cost
#La matriz de costos es:
WW2$costMatrix
#La matriz con el analisis final es:
WW2$analysis

#costo de setup y holding
holding<-sum(WW2$analysis[7,])
setup<-sum(WW2$analysis[6,])
question8<-holding+setup
question8

#si se reduce el 50% de K
WW3<-WW(lambda = lambda, c=c, h=h, K=K*.5)
#El costo total sera de:
WW3$cost
#La matriz de costos es:
WW3$costMatrix
#La matriz con el analisis final es:
WW3$analysis

#Para ver si conviente invertir
if(WW2$cost-1500>WW3$cost){print("Si conviene invertir")}else{print("no conviene invertir")}
#entonces no conviene invertir




