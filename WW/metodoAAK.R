#
#@adendak 
#primer intento de entender como funciona el metodo WW, se automatizara despues
#
x<-c(60,100,10,200,120,15) #lambda
n  <- length(x)#Calculating the output matrix (costs matrix)
a<-c(150,110,120,200,200,200) #costo fijo
h<-c(.8, .8,.8,1.8,2,2) #holding cost
c<-rep(5,n) #costo
CM<- matrix(NA,nrow=n,ncol=n) #matriz resultado
f<- matrix(NA,nrow=1,ncol=n+1) #minimo de la columna, tiene un 0 al principio
hc<-matrix(NA,nrow=n,ncol=n) #holding cost de cada caso

c<-c*x #considerando que el costo es constante, andy sabe cambiarlo
#[row, col]
#primer renglon. 
#esto va afuera del for de columnas
#row<-1
hc[1,1]<-0
f[1]<-0
CM[1,1]<-c[1]+a[1]

#adentro del for de columnas, va de (1=row)+1:n 
hc[1,2]<-hc[1,1]+sum(x[2]*h[1:1]) #80
CM[1,2]<-sum(c[1:2])+hc[1,2]+a[1]+f[1]

hc[1,3]<-hc[1,2]+sum(x[3]*h[1:2]) #96
CM[1,3]<-sum(c[1:3])+hc[1,3]+a[1]+f[1]

hc[1,4]<-hc[1,3]+sum(x[4]*h[1:3])#576
CM[1,4]<-sum(c[1:4])+hc[1,4]+a[1]+f[1]

hc[1,5]<-hc[1,4]+sum(x[5]*h[1:4])
CM[1,5]<-sum(c[1:5])+hc[1,5]+a[1]+f[1]

hc[1,6]<-hc[1,5]+sum(x[6]*h[1:5])
CM[1,6]<-sum(c[1:6])+hc[1,6]+a[1]+f[1]

f[2]<-min(CM[,1], na.rm=TRUE)

#para el segundo renglon
hc[2,2]<-0
CM[2,2]<-c[2]+a[2]+f[2]

hc[2,3]<-hc[2,2]+sum(x[3]*h[2:2]) #8
CM[2,3]<-sum(c[2:3])+hc[2,3]+a[2]+f[2] #1118

hc[2,4]<-hc[2,3]+sum(x[4]*h[2:3]) #328
CM[2,4]<-sum(c[2:4])+hc[2,4]+a[2]+f[2] #2438

hc[2,5]<-hc[2,4]+sum(x[5]*h[2:4]) #736
CM[2,5]<-sum(c[2:5])+hc[2,5]+a[2]+f[2] #3446

hc[2,6]<-hc[2,5]+sum(x[6]*h[2:5]) #817
CM[2,6]<-sum(c[2:6])+hc[2,6]+a[2]+f[2] #3602

f[3]<-min(CM[,2],na.rm=TRUE)


#tercer renglon
#manual
hc[3,3]<-0
CM[3,3]<-c[3]+a[3]+f[3]

hc[3,4]<-hc[3,3]+sum(x[4]*h[3:3]) #328
CM[3,4]<-sum(c[2:4])+hc[2,4]+a[2]+f[3] #2438

hc[3,5]<-hc[3,4]+sum(x[5]*h[3:4]) #736
CM[3,5]<-sum(c[3:5])+hc[3,5]+a[3]+f[3] #3446

hc[3,6]<-hc[3,5]+sum(x[6]*h[3:5]) #817
CM[3,6]<-sum(c[3:6])+hc[3,6]+a[3]+f[3] #3602

f[4]<-min(CM[,3],na.rm=TRUE)

#para el 4to 
#manual
hc[4,4]<-0
CM[4,4]<-c[4]+a[4]+f[4]

hc[4,5]<-hc[4,4]+sum(x[5]*h[4:4]) #736
CM[4,5]<-sum(c[4:5])+hc[4,5]+a[4]+f[4] #3446

hc[4,6]<-hc[4,5]+sum(x[6]*h[4:5]) #817
CM[4,6]<-sum(c[4:6])+hc[4,6]+a[4]+f[4] #3602

f[5]<-min(CM[,4],na.rm=TRUE)

#para el 5to 
#manual
hc[5,5]<-0
CM[5,5]<-c[5]+a[5]+f[5]

hc[5,6]<-hc[5,5]+sum(x[6]*h[5:5]) #817
CM[5,6]<-sum(c[5:6])+hc[5,6]+a[5]+f[5] #3602

f[6]<-min(CM[,5],na.rm=TRUE)

#para el 6to
CM[6,6]<-c[6]+a[6]+f[6]

f[7]<-min(CM[,6],na.rm=TRUE)

#-------------------------------------------------------------------------------
# Empezando a automatizar
#-------------------------------------------------------------------------------
#ahora con diferentes for
aux<- matrix(NA,nrow=n,ncol=n)
#primer intento de un for
hcAux<-matrix(NA,nrow=n,ncol=n)
hcAux[1,1]<-0
aux[1,1]<-sum(c[1:1])+a[1]
for (col in 2:n){
  hcAux[1,col]<-hcAux[1,col-1]+sum(x[col]*h[1:col-1])
  print(hcAux)
  aux[1,col]<-sum(c[1:col])+a[1]+hcAux[1,col] 
  print(aux)
}
aux==CM
#con esto ya llenamos el primer renglon

#segundo renglon
hcAux<-matrix(NA,nrow=n,ncol=n)
hcAux[2,2]<-0
aux[2,2]<-c[2]+a[2]+f[2]
for (col in 2+1:n){
  hcAux[2,col]<-hcAux[2,(col-1)]+sum(x[col]*h[2:(col-1)])
  #print(hcAux)
  aux[2,col]<-sum(c[2:col])+a[2]+hcAux[2,col]+f[2] 
  #print(aux)
}
aux==CM

#tercer renglon
#ESTE NO SIRVE AIUDA
hcAux[3,3]<-0
aux[3,3]<-c[3]+a[3]+f[3]
for (col in 3+1:n-1){
  hcAux[3,col]<-hcAux[3,(col-1)]+sum(x[col]*h[3:(col-1)])
  #print(hcAux)
  print(col)
  aux[3,col]<-sum(c[3:col])+a[3]+hcAux[3,col]+f[3] 
  #print(aux)
}
aux==CM

#cuarto
#este si jala 
hcAux[4,4]<-0
aux[4,4]<-c[4]+a[4]+f[4]
for (col in 4+1:n){
  hcAux[4,col]<-hcAux[4,(col-1)]+sum(x[col]*h[4:(col-1)])
  print(col)
  aux[4,col]<-sum(c[4:col])+a[4]+hcAux[4,col]+f[4] 
  #print(aux)
}
aux==CM

#quinto
#este si jala 
hcAux[5,5]<-0
aux[5,5]<-c[5]+a[5]+f[5]
for (col in 5+1:n){
  hcAux[5,col]<-hcAux[5,(col-1)]+sum(x[col]*h[5:(col-1)])
  print(col)
  aux[5,col]<-sum(c[5:col])+a[5]+hcAux[5,col]+f[5] 
  #print(aux)
}
aux==CM

#sexto
#este si jala 
hcAux[6,6]<-0
aux[6,6]<-c[6]+a[6]+f[6]
for (col in 6+1:n){
  hcAux[6,col]<-hcAux[6,(col-1)]+sum(x[col]*h[6:(col-1)])
  print(col)
  aux[6,col]<-sum(c[6:col])+a[6]+hcAux[6,col]+f[6] 
  #print(aux)
}
aux==CM

#el renglon 3 no jala. 
#hay que hacer un for gigante para cambiar en cada uno ese numero por un row. 
#ya lo intente y no sirve








                       
