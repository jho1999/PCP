#
#
#
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

#Primer ejercicio
lambda<- c(60 ,	100 ,	10 ,	200 ,	120 ,	15)

#relga de peterson-silver
#V=n*sum(lamda^2)/(sum(lambda))^2-1

peterson<-length(lambda)*sum(lambda^2)/(sum(lambda))^2-1

if(peterson>0.5){paste0("Usar dynamicLotSizing, el res de Peterson-Silver es:", peterson)}

h<-c(1,1,2,2,2,2)
c<-c(5,5,6,6,7,7)
K<-c(150,150,150,100,100,100)

WW1<-WW(lambda = lambda, c=c, h=h, K=K)
#El costo total sera de:
WW1$cost
#La matriz de costos es:
WW1$costMatrix
#La matriz con el analisis final es:
WW1$analysis

inventario<-c()
inventario[1]<-periodo1-lambda[1]
invenratio[2]<-inventario[2]-lambda[2]
inventario[3]<-invenratio[3]-lambda[3]
inventario[4]<-periodo4-lambda[4]
inventario[5]<-periodo5-lambda[5]
inventario[6]<-inventario[5]-lambda-6


