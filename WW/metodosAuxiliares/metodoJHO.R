##Pruebas con Ejercicio 3 de "Examples Dynamic Lot Sizing"
#Costo = 3201
#Q5 = lam5 + lam6
#Q4 = lam4
#Q1 = lam1 + lam2 + lam3

#params de la funcion
lambda = c(60,100,10,200,120,15)
h = c(.8,.8,.8,1.8,2,2)
c = c(5,5,5,5,5,5)
K = c(150,110,120,200,200,200)

#Declaraciones inciales (valores iniciales)
n=length(lambda) #numero de periodos que considero (número periodos de la demanda)
ctoMinAnt=0  #inicializo costo en cero

#Declaración de matrices
mCto=matrix(data = 0, nrow = n, ncol = n) #declaro lo que será la matriz de costos con valor de cero para facilitar
mHoldCto=matrix(data = 0, nrow = n, ncol = n) #declaro una matriz de holding cost por cada opción (depende de lambda, inventario y periodo)

#Asignación valores a las matrices 
#Lleno matriz de holding cost
#Lleno por renglones
for(ren in 1:(n-1)){ #recorro los renglones del 1 al n-1 por que en n no hay holding cost
  for(col in (ren+1):n){ #recorro las columnas del renglon en que esta +1 hasta n
    mHoldCto[ren,col]=mHoldCto[ren,(col-1)]+sum(h[ren:(col-1)])*lambda[col]
  } #a cada celda entrada le asigna el valor dado por el holding cost acumulado del periodo anterior + holding cost de este periodo 
}


#Lleno la matriz de costos
#Lleno por renglones
for(ren in 1:n){  #recorro los renglones del primer al último periodo
  for(col in ren:n){ #recorro las columnas de donde esoy hasta n
    mCto[ren,col]=ctoMinAnt+K[ren]+c[ren]*sum(lambda[ren:col])+mHoldCto[ren,col] 
    #para cada entrada calculo y guardo el costo de la forma costo minimo anterior + 
    #costo*lambdas anteriores + holding cost de ese periodo (calculado en la matriz hc)
  }
  ctoMinAnt=min(mCto[1:ren,ren])  #Guardamos en la variable auxiliar el costo minimo del periodo anterior para los calculos
}

#resultados
CostoTotal=ctoMinAnt
CostoTotal
mCto #matriz de costos

#Para poner bonitos los resultados
i=n
Q=rep(0,n) #Creo vector para meter los valores de Q
while(i>0){
  minPer=which.min(mCto[1:i,i]) #busca costo menor del periodo y decide en que periodo se pide
  print(paste("Se pide en ",minPer," para ",i))
  Q[minPer]=sum(lambda[minPer:i])
  i=minPer-1 #se va al periodo anterior que se tenga que analizar
}
Q