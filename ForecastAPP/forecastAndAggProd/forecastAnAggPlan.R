library(fpp2)

forecastToh <- function(ts,endTs=c(2020,12),f,hF){
  
  if(is.null(ts))
    return(NULL)
  
  pValue <- Box.test(ts, lag = 1, type = "Ljung")$p.value
  
  if(pValue <= 0.05){
    
    endTrainData=endTs
    for(i in 1:hF){
      if(endTrainData[2]<=1){
        endTrainData[1]=endTrainData[1]-1
        endTrainData[2]=f
      }else{
        endTrainData[2]=endTrainData[2]-1
      }
      if(i==hF-1){
        startTestData=endTrainData
      }
    }
    
    trainData=window(ts,end=endTrainData)
    testData=window(ts,start = startTestData)
    
    
    # #------------------------------------------
    # #Encontrar el mejor método para pronosticar
    # #------------------------------------------
   
    # #---------Hacer pronósticos-----
    averageMethod <- meanf(trainData, h=hF) 
    naiveMethod <- naive(trainData, h= hF) 
    seasonalNaiveMethod <- snaive(trainData, h=hF)
    fcSimpleExpSmoo <- ses(trainData, initial = c("optimal"), h= hF) 
    holtForecast <- holt(trainData, h=hF)
    hwAditive <- hw(trainData, seasonal = "additive", h= hF)
    hwMultiplicative <- hw(trainData, seasonal = "multiplicative", h = hF)
    # 
    # #-------Checar la exactitud de los métodos
    accAve=accuracy(averageMethod, testData)
    accNai=accuracy(naiveMethod, testData)
    accSNai=accuracy(seasonalNaiveMethod, testData)
    accExp=accuracy(fcSimpleExpSmoo, testData)
    accHolt=accuracy(holtForecast, testData)
    accHWA=accuracy(hwAditive, testData)
    accHWM=accuracy(hwMultiplicative, testData)
    
    meths=c(accAve[1,2],accNai[1,2],accSNai[1,2],accExp[1,2],accHolt[1,2],accHWA[1,2],accHWM[1,2])
    
    cond=TRUE
    while(cond){
      minRMSE=which.min(meths)
      
      if(meths[minRMSE]==Inf){
        warning("NO METHOD FITS THE DATA")
        return(NULL)
      }
      
      selectedMethod=switch (minRMSE,
                             meanf(ts,h=hF),
                             naive(ts,h=hF),
                             snaive(ts,h=hF),
                             ses(ts,initial = c("optimal"),h=hF),
                             holt(ts,h=hF),
                             hw(ts, seasonal = "additive", h= hF),
                             hw(ts, seasonal = "multiplicative", h= hF)
      )
      
      pValueSelected=Box.test(selectedMethod$residuals, lag = 1, type = "Ljung")$p.value
      
      if(pValueSelected<=0.05){
        meths[minRMSE]=Inf
      }else{
        cond=FALSE
      }
    }
    return(selectedMethod)
    
  }else{
    warning("Time series is random")
    return(NULL)
  }
  
}

require(lpSolve)

varNames <- function(tP=6){
  varLet=c('W','H','F','I','B','P')
  varNames=c('W0','I0','B0')
  for(i in 1:tP){
    for(j in 1:6){
      varNames=c(varNames,paste(varLet[j],i,sep=""))
    }
  }
  return(varNames)   
}

restNames <- function(tP=6,mode=1){
  #mode:
  #1=base model
  #2=level workforce
  #3=no backorders
  #4=zero inventory and backorders
  
  varLet=c('Wo','In','Pr')
  
  varLet=switch(mode,varLet,c(varLet,'Wk'),c(varLet,'Wk','Bkz'),c(varLet,'Inz','Bkz'))
  
  varNames=c('InitialWorkers','InitialInventory','InitialBackorders')
  for(i in 1:length(varLet)){
    for(j in 1:tP){
      varNames=c(varNames,paste(varLet[i],j,sep=""))
    }
  }
  return(varNames)   
}

aggBaseModel <- function(lambda=c(0,0,0,0,0,0),tPeriods=6,lDays=c(22,22,22,22,22,22),W0=0,I0=0,B0=0,wages=0,hire=0,fire=0,hold=0,backOrder=0,pCost=0,pLastPeriod=0,wLastPeriod=0,dLastPeriod=0){
  
  numVar=tPeriods*6+3
  numRes=tPeriods*3+3
  
  vars=varNames(tP = tPeriods)
  rest=restNames(tP = tPeriods,mode=1)
  
  monthWage=wages*lDays
  objF=c(0,0,0)
  for(i in 1:tPeriods)
    objF=c(objF,monthWage[i],hire,fire,hold,backOrder,pCost)
  
  lpMatrix = matrix(0, nrow = numRes, ncol = numVar, dimnames = list(rest, vars))
  
  lpMatrix['InitialWorkers', 'W0'] = 1
  lpMatrix['InitialInventory', 'I0'] = 1
  lpMatrix['InitialBackorders', 'B0'] = 1
  
  initialSigns = rep("=", 3)
  
  InitialNumberOfWorkers = W0
  InitialNumberInventory = I0
  InitialNumberBackorders = B0
  
  initialsRHS = c(InitialNumberOfWorkers,
                  InitialNumberInventory,
                  InitialNumberBackorders)
  
  lpMatrix['Wo1','W0']=-1
  lpMatrix['In1','I0']=-1
  lpMatrix['In1','B0']=1
  for(j in 2:tPeriods){
    lpMatrix[rest[j+3],vars[4+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[7+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[8+6*(j-2)]]=1
  }
  
  for(k in 1:tPeriods){
    lpMatrix[rest[k+3],vars[5+6*(k-1)]]=-1
    lpMatrix[rest[k+3],vars[4+6*(k-1)]]=1
    lpMatrix[rest[k+3],vars[6+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[7+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[8+6*(k-1)]]=-1
    lpMatrix[rest[k+3+tPeriods],vars[9+6*(k-1)]]=-1
    lpMatrix[rest[k+3+2*tPeriods],vars[9+6*(k-1)]]=1
    lpMatrix[rest[k+3+2*tPeriods],vars[4+6*(k-1)]]=-(pLastPeriod/(dLastPeriod*wLastPeriod))*lDays[k]
  }
  
  workersSigns = rep("=", tPeriods)
  workersRHS = rep(0,tPeriods)
  inventorySigns = rep("=", tPeriods)
  inventoryRHS = -1*lambda
  productionSigns = rep("=", tPeriods)
  productionRHS = rep(0,tPeriods)
  
  # Find the optimal solution
  aggPlan = lp(direction = "min", objective.in = objF, const.mat = lpMatrix, const.dir = c(initialSigns,workersSigns,inventorySigns,productionSigns), const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS))
  
  #Print solution
  print(paste("Total cost = ", aggPlan$objval))
  best_sol = aggPlan$solution
  names(best_sol) = vars
  print(best_sol)
  
  return(aggPlan)
}

aggLevelWorkforce <- function(lambda=c(0,0,0,0,0,0),tPeriods=6,lDays=c(22,22,22,22,22,22),W0=0,I0=0,B0=0,wages=0,hire=0,fire=0,hold=0,backOrder=0,pCost=0,pLastPeriod=0,wLastPeriod=0,dLastPeriod=0,levelWf=0){
  
  numVar=tPeriods*6+3
  numRes=tPeriods*4+3
  
  vars=varNames(tP = tPeriods)
  rest=restNames(tP = tPeriods,mode=2)
  
  monthWage=wages*lDays
  objF=c(0,0,0)
  for(i in 1:tPeriods)
    objF=c(objF,monthWage[i],hire,fire,hold,backOrder,pCost)
  
  lpMatrix = matrix(0, nrow = numRes, ncol = numVar, dimnames = list(rest, vars))
  
  lpMatrix['InitialWorkers', 'W0'] = 1
  lpMatrix['InitialInventory', 'I0'] = 1
  lpMatrix['InitialBackorders', 'B0'] = 1
  
  initialSigns = rep("=", 3)
  
  InitialNumberOfWorkers = W0
  InitialNumberInventory = I0
  InitialNumberBackorders = B0
  
  initialsRHS = c(InitialNumberOfWorkers,
                  InitialNumberInventory,
                  InitialNumberBackorders)
  
  lpMatrix['Wo1','W0']=-1
  lpMatrix['In1','I0']=-1
  lpMatrix['In1','B0']=1
  for(j in 2:tPeriods){
    lpMatrix[rest[j+3],vars[4+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[7+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[8+6*(j-2)]]=1
  }
  
  for(k in 1:tPeriods){
    lpMatrix[rest[k+3],vars[5+6*(k-1)]]=-1
    lpMatrix[rest[k+3],vars[4+6*(k-1)]]=1
    lpMatrix[rest[k+3],vars[6+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[7+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[8+6*(k-1)]]=-1
    lpMatrix[rest[k+3+tPeriods],vars[9+6*(k-1)]]=-1
    lpMatrix[rest[k+3+2*tPeriods],vars[9+6*(k-1)]]=1
    lpMatrix[rest[k+3+2*tPeriods],vars[4+6*(k-1)]]=-(pLastPeriod/(dLastPeriod*wLastPeriod))*lDays[k]
    lpMatrix[rest[k+3+3*tPeriods],vars[4+6*(k-1)]]=1
  }
  
  workersSigns = rep("=", tPeriods)
  workersRHS = rep(0,tPeriods)
  inventorySigns = rep("=", tPeriods)
  inventoryRHS = -1*lambda
  productionSigns = rep("=", tPeriods)
  productionRHS = rep(0,tPeriods)
  invBackSigns = rep("=", tPeriods)
  invBackRHS = rep(levelWf,tPeriods)
  
  # Find the optimal solution
  aggPlan = lp(direction = "min", objective.in = objF, const.mat = lpMatrix, const.dir = c(initialSigns,workersSigns,inventorySigns,productionSigns,invBackSigns), const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, invBackRHS))
  
  #Print solution
  print(paste("Total cost = ", aggPlan$objval))
  best_sol = aggPlan$solution
  names(best_sol) = vars
  print(best_sol)
  
  return(aggPlan)
}

aggNoBackorder <- function(lambda=c(0,0,0,0,0,0),tPeriods=6,lDays=c(22,22,22,22,22,22),W0=0,I0=0,B0=0,wages=0,hire=0,fire=0,hold=0,backOrder=0,pCost=0,pLastPeriod=0,wLastPeriod=0,dLastPeriod=0,levelWf=0){
  
  numVar=tPeriods*6+3
  numRes=tPeriods*5+3
  
  vars=varNames(tP = tPeriods)
  rest=restNames(tP = tPeriods,mode=3)
  
  monthWage=wages*lDays
  objF=c(0,0,0)
  for(i in 1:tPeriods)
    objF=c(objF,monthWage[i],hire,fire,hold,backOrder,pCost)
  
  lpMatrix = matrix(0, nrow = numRes, ncol = numVar, dimnames = list(rest, vars))
  
  lpMatrix['InitialWorkers', 'W0'] = 1
  lpMatrix['InitialInventory', 'I0'] = 1
  lpMatrix['InitialBackorders', 'B0'] = 1
  
  initialSigns = rep("=", 3)
  
  InitialNumberOfWorkers = W0
  InitialNumberInventory = I0
  InitialNumberBackorders = B0
  
  initialsRHS = c(InitialNumberOfWorkers,
                  InitialNumberInventory,
                  InitialNumberBackorders)
  
  lpMatrix['Wo1','W0']=-1
  lpMatrix['In1','I0']=-1
  lpMatrix['In1','B0']=1
  for(j in 2:tPeriods){
    lpMatrix[rest[j+3],vars[4+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[7+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[8+6*(j-2)]]=1
  }
  
  for(k in 1:tPeriods){
    lpMatrix[rest[k+3],vars[5+6*(k-1)]]=-1
    lpMatrix[rest[k+3],vars[4+6*(k-1)]]=1
    lpMatrix[rest[k+3],vars[6+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[7+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[8+6*(k-1)]]=-1
    lpMatrix[rest[k+3+tPeriods],vars[9+6*(k-1)]]=-1
    lpMatrix[rest[k+3+2*tPeriods],vars[9+6*(k-1)]]=1
    lpMatrix[rest[k+3+2*tPeriods],vars[4+6*(k-1)]]=-(pLastPeriod/(dLastPeriod*wLastPeriod))*lDays[k]
    lpMatrix[rest[k+3+3*tPeriods],vars[4+6*(k-1)]]=1
    lpMatrix[rest[k+3+4*tPeriods],vars[8+6*(k-1)]]=1
  }
  
  workersSigns = rep("=", tPeriods)
  workersRHS = rep(0,tPeriods)
  inventorySigns = rep("=", tPeriods)
  inventoryRHS = -1*lambda
  productionSigns = rep(">=", tPeriods)
  productionRHS = rep(0,tPeriods)
  invBackSigns <- rep("=", 2*tPeriods)
  invBackRHS <- c(rep(levelWf,tPeriods), rep(0,tPeriods))
  
  # Find the optimal solution
  aggPlan = lp(direction = "min", objective.in = objF, const.mat = lpMatrix, const.dir = c(initialSigns,workersSigns,inventorySigns,productionSigns,invBackSigns), const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, invBackRHS))
  
  #Print solution
  print(paste("Total cost = ", aggPlan$objval))
  best_sol = aggPlan$solution
  names(best_sol) = vars
  print(best_sol)
  
  return(aggPlan)
}

aggZeroInvBack <- function(lambda=c(0,0,0,0,0,0),tPeriods=6,lDays=c(22,22,22,22,22,22),W0=0,I0=0,B0=0,wages=0,hire=0,fire=0,hold=0,backOrder=0,pCost=0,pLastPeriod=0,wLastPeriod=0,dLastPeriod=0){
  
  numVar=tPeriods*6+3
  numRes=tPeriods*5+3
  
  vars=varNames(tP = tPeriods)
  rest=restNames(tP = tPeriods,mode=4)
  
  monthWage=wages*lDays
  objF=c(0,0,0)
  for(i in 1:tPeriods)
    objF=c(objF,monthWage[i],hire,fire,hold,backOrder,pCost)
  
  lpMatrix = matrix(0, nrow = numRes, ncol = numVar, dimnames = list(rest, vars))
  
  lpMatrix['InitialWorkers', 'W0'] = 1
  lpMatrix['InitialInventory', 'I0'] = 1
  lpMatrix['InitialBackorders', 'B0'] = 1
  
  initialSigns = rep("=", 3)
  
  InitialNumberOfWorkers = W0
  InitialNumberInventory = I0
  InitialNumberBackorders = B0
  
  initialsRHS = c(InitialNumberOfWorkers,
                  InitialNumberInventory,
                  InitialNumberBackorders)
  
  lpMatrix['Wo1','W0']=-1
  lpMatrix['In1','I0']=-1
  lpMatrix['In1','B0']=1
  for(j in 2:tPeriods){
    lpMatrix[rest[j+3],vars[4+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[7+6*(j-2)]]=-1
    lpMatrix[rest[j+3+tPeriods],vars[8+6*(j-2)]]=1
  }
  
  for(k in 1:tPeriods){
    lpMatrix[rest[k+3],vars[5+6*(k-1)]]=-1
    lpMatrix[rest[k+3],vars[4+6*(k-1)]]=1
    lpMatrix[rest[k+3],vars[6+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[7+6*(k-1)]]=1
    lpMatrix[rest[k+3+tPeriods],vars[8+6*(k-1)]]=-1
    lpMatrix[rest[k+3+tPeriods],vars[9+6*(k-1)]]=-1
    lpMatrix[rest[k+3+2*tPeriods],vars[9+6*(k-1)]]=1
    lpMatrix[rest[k+3+2*tPeriods],vars[4+6*(k-1)]]=-(pLastPeriod/(dLastPeriod*wLastPeriod))*lDays[k]
    lpMatrix[rest[k+3+3*tPeriods],vars[7+6*(k-1)]]=1
    lpMatrix[rest[k+3+4*tPeriods],vars[8+6*(k-1)]]=1
  }
  
  workersSigns = rep("=", tPeriods)
  workersRHS = rep(0,tPeriods)
  inventorySigns = rep("=", tPeriods)
  inventoryRHS = -1*lambda
  productionSigns = rep("<=", tPeriods)
  productionRHS = rep(0,tPeriods)
  invBackSigns <- rep("=", 2*tPeriods)
  invBackRHS <- rep(0,2*tPeriods)
  
  # Find the optimal solution
  aggPlan = lp(direction = "min", objective.in = objF, const.mat = lpMatrix, const.dir = c(initialSigns,workersSigns,inventorySigns,productionSigns,invBackSigns), const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, invBackRHS))
  
  #Print solution
  print(paste("Total cost = ", aggPlan$objval))
  best_sol = aggPlan$solution
  names(best_sol) = vars
  print(best_sol)
  
  return(aggPlan)
}
restNames(tP=6,mode=4)

varNames(tP=6)