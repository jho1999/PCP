require(lpSolve)

numberVariables = 27
numberConstraints = 21

variablesNames = c('W0','I0','B0', 
                   'P1','W1','H1','F1','I1','B1',
                   'P2','W2','H2','F2','I2','B2',
                   'P3','W3','H3','F3','I3','B3',
                   'P4','W4','H4','F4','I4','B4')

constrainstNames = c('InitW', 'InitIn', 'InitBk',
                     'Wo1','Wo2', 'Wo3','Wo4',
                     'In1', 'In2','In3','In4',
                     'Pr1','Pr2','Pr3','Pr4',
                     'Bk1','Bk2','Bk3','Bk4',
                     'Inz4','W1W3')


unitsPerWorkPerTri = ceiling(7659/(254*10))
lambda<-c(632,690,858,772)
lambdaAux<-cumsum(lambda)
totLambda<-sum(lambda) #para no backorder usamos lambda aux
workingDays<-c(24,26,24,20)
totWorkingDays<-sum(workingDays)
workersNeeded<-totLambda/(totWorkingDays*unitsPerWorkPerTri)

# Declara la función objetivo
objFunction <- c(0,0,0,
                 500,6000,300,600,50,75, #oct
                 500,6000,300,600,50,75, #nov
                 500,6000,300,600,50,75, #dic
                 500,6000,300,600,50,75) #ene


A <- matrix(0, nrow = numberConstraints, ncol = numberVariables, 
            dimnames = list(constrainstNames, variablesNames)
)




A['InitW', 'W0'] = 1
A['InitIn', 'I0'] = 1
A['InitBk', 'B0'] = 1

intialSigns <- rep("=", 3)

initialsRHS <- c(10,
                 0,
                 50)

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

A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -unitsPerWorkPerTri*workingDays[1];
A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -unitsPerWorkPerTri*workingDays[2];
A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -unitsPerWorkPerTri*workingDays[3];
A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -unitsPerWorkPerTri*workingDays[4];

productionSigns <- rep("<=", 4)
productionRHS <- rep(0,4)

A['Bk1', 'B1'] = 1
A['Bk2', 'B2'] = 1
A['Bk3', 'B3'] = 1
A['Bk4', 'B4'] = 1

bkSigns <- rep("=", 4)
bkRHS <- rep(0,4)

#inventario cero del periodo 4
A['Inz4','I4'] = 1;

inz4Sign = c("=")
inz4RHS = c(0)
#DUDA 

#por cada trabajador en octubre, por lo menos 2 en diciembre

A['W1W3','W1']=2; A['W1W3','W3']=-1;

w1w3Signs=c("<=")
w1w3RHS=c(0)


# Find the optimal solution
#agregar las que faltan 
aggregateProductionPlan <-  lp(direction = "min",
                               objective.in = objFunction,
                               const.mat = A,
                               const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns, bkSigns,inz4Sign,w1w3Signs),
                               const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, bkRHS, inz4RHS,w1w3RHS),
                               int.vec = c(5,11,17,23) # Wi = integers
)

#Print solution
print(paste("The total cost is: ", aggregateProductionPlan$objval))
best_sol <- aggregateProductionPlan$solution
names(best_sol) <- variablesNames
print(best_sol)
