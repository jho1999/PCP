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

restNames <- function(tP=6){
  varLet=c('Wo','In','Pr')
  varNames=c('InitialWorkers','InitialInventory','InitialBackorders')
  for(i in 1:3){
    for(j in 1:tP){
      varNames=c(varNames,paste(varLet[i],j,sep=""))
    }
  }
  return(varNames)   
}

aggPlanPt <- function(lambda=c(0,0,0,0,0,0),tPeriods=6,lDays=c(22,22,22,22,22,22),W0=0,I0=0,B0=0,wages=0,hire=0,fire=0,hold=0,backOrder=0,pCost=0,pLastPeriod=0,wLastPeriod=0,dLastPeriod=0){
  
  numVar=tPeriods*6+3
  numRes=tPeriods*3+3
  
  vars=varNames(tP = tPeriods)
  rest=restNames(tP = tPeriods)
  
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
    lpMatrix[rest[j+9],vars[7+6*(j-2)]]=-1
    lpMatrix[rest[j+9],vars[8+6*(j-2)]]=1
  }
  
  for(k in 1:tPeriods){
    lpMatrix[rest[k+3],vars[5+6*(k-1)]]=-1
    lpMatrix[rest[k+3],vars[4+6*(k-1)]]=1
    lpMatrix[rest[k+3],vars[6+6*(k-1)]]=1
    lpMatrix[rest[k+9],vars[7+6*(k-1)]]=1
    lpMatrix[rest[k+9],vars[8+6*(k-1)]]=-1
    lpMatrix[rest[k+9],vars[9+6*(k-1)]]=-1
    lpMatrix[rest[k+15],vars[9+6*(k-1)]]=1
    lpMatrix[rest[k+15],vars[4+6*(k-1)]]=-(pLastPeriod/(dLastPeriod*wLastPeriod))*lDays[k]
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

test=aggPlanPt(lambda=c(2760,3320,3970,3540,3180,2900),tPeriods=6,lDays=c(21,20,23,21,22,22),W0=35,I0=0,B0=0,wages=120,hire=450,fire=600,hold=5,backOrder=15,pCost=0,pLastPeriod=41383,wLastPeriod=40,dLastPeriod=260)