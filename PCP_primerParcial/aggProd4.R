require(lpSolve)

numberVariables = 31
numberConstraints = 21

variablesNames = c('W0','I0','B0', 
                   'P1','W1','H1','F1','I1','B1','O1',
                   'P2','W2','H2','F2','I2','B2','O2',
                   'P3','W3','H3','F3','I3','B3','O3',
                   'P4','W4','H4','F4','I4','B4','O4')

constrainstNames = c('InitW', 'InitIn', 'InitBk',
                     'Wo1','Wo2', 'Wo3','Wo4',
                     'In1', 'In2','In3','In4',
                     'Pr1','Pr2','Pr3','Pr4',
                     'Bk1','Bk2','Bk3','Bk4',
                     'In4','I3I1')


aux = ceiling(7449/(253*19))
#aux<-aux*3 #es por dia
lambda<-c(1861,1941,1793,1763)
#lambdaAux<-cumsum(lambda)
#totLambda<-sum(lambda) #para no backorder usamos lambda aux
workingDays<-c(62,62,66,63)
#totWorkingDays<-sum(workingDays)
workersNeeded<-totLambda/(totWorkingDays*aux)

# Declara la función objetivo
objFunction <- c(0,0,0,
                 1000,6000,1000,2000,500,0,1100, #Q1
                 1000,6000,1000,2000,500,0,1100, #Q2
                 1000,6000,1000,2000,500,0,1100, #Q3
                 1000,6000,1000,2000,500,0,1100) #Q4
objFunction<-objFunction*3 #por que todo es mensual 


A <- matrix(0, nrow = numberConstraints, ncol = numberVariables, 
            dimnames = list(constrainstNames, variablesNames)
)




A['InitW', 'W0'] = 1
A['InitIn', 'I0'] = 1
A['InitBk', 'B0'] = 1

intialSigns <- rep("=", 3)

initialsRHS <- c(19,
                 1500,
                 0)

A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
A['Wo4', 'W4'] = 1; A['Wo4', 'W3'] = -1; A['Wo4', 'H4'] = -1; A['Wo4', 'F4'] = 1;

workersSigns <- rep("=", 4)

workersRHS <- rep(0,4)

A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1;
A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1;
A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1;
A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1;

inventorySigns <- rep("=", 4)

inventoryRHS <- -lambda

A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -aux*workingDays[1]; A['Pr1', 'O1'] = -1
A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -aux*workingDays[2]; A['Pr2', 'O2'] = -1
A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -aux*workingDays[3]; A['Pr3', 'O3'] = -1
A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -aux*workingDays[4]; A['Pr4', 'O4'] = -1

productionSigns <- rep("=", 4)
productionRHS <- rep(0,4)

A['Bk1', 'B1'] = 1
A['Bk2', 'B2'] = 1
A['Bk3', 'B3'] = 1
A['Bk4', 'B4'] = 1

bkSigns <- rep("=", 4)
bkRHS <- rep(0,4)

#inventario del periodo 4
A['In4','I4'] = 1;

in4Sign = c("=")
in4RHS = c(2000)
#DUDA 

#por cada trabajador en octubre, por lo menos 2 en diciembre

A['I3I1','I1']=3; A['I3I1','I1']=-1;

i3i1Signs=c("<=")
i3i1RHS=c(0)


# Find the optimal solution
#agregar las que faltan 
aggregateProductionPlan <-  lp(direction = "min",
                               objective.in = objFunction,
                               const.mat = A,
                               const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns, bkSigns,in4Sign,i3i1Signs),
                               const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, bkRHS, in4RHS,i3i1RHS),
                               #int.vec = c(5,12,19,26) # Wi = integers (cambian por el Oi)
)

#Print solution
print(paste("The total cost is: ", aggregateProductionPlan$objval))
best_sol <- aggregateProductionPlan$solution
names(best_sol) <- variablesNames
print(best_sol)
