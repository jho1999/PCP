require(lpSolve)

numberVariables = 31
numberConstraints = 21

variablesNames = c('W0','I0','B0',
                   'W1','H1','F1','I1','B1','P1', 'O1',
                   'W2','H2','F2','I2','B2','P2', 'O2',
                   'W3','H3','F3','I3','B3','P3', 'O3',
                   'W4','H4','F4','I4','B4','P4', 'O4')

constrainstNames = c('InitW', 'InitIn', 'InitBk',
                     'Wo1','Wo2', 'Wo3','Wo4',
                     'In1', 'In2','In3','In4',
                     'Pr1','Pr2','Pr3','Pr4',
                     'Bk1z','Bk2z','Bk3z','Bk4z',
                     'Inv4','I3I1')

lambda <- c(1861,1941,1793,1763)
workingDays<-c(62,62,66,63)

aux = ceiling(7449/(253*19))


cW1 = 6000 
cW2 = 6000
cW3 = 6000
cW4 = 6000

InitialNumberOfWorkers = 19
InitialInventory=1500
InitialBackorders= 0

#Definir aqui los costos
cH= 1000
cF= 2000
cI= 500
cB = 0
cP = 1000
cO = 1100


objFunction <- c(0,0,0,
                 cW1,cH,cF,cI,cB,cP,cO,
                 cW2,cH,cF,cI,cB,cP, cO,
                 cW3 ,cH,cF,cI,cB,cP, cO,
                 cW4,cH,cF,cI,cB,cP, cO)

objFunction<-objFunction*3

A <- matrix(0, nrow = numberConstraints, ncol = numberVariables, 
            dimnames = list(constrainstNames, variablesNames))

A['InitW', 'W0'] = 1
A['InitIn', 'I0'] = 1
A['InitBk', 'B0'] = 1

intialSigns <- rep("=", 3)

initialsRHS <- c(InitialNumberOfWorkers,
                 InitialInventory,
                 InitialBackorders)

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

inventoryRHS <- c(-1*lambda)

A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -aux*workingDays[1]; A['Pr1', 'O1'] = -1
A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -aux*workingDays[2]; A['Pr2', 'O2'] = -1
A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -aux*workingDays[3]; A['Pr3', 'O3'] = -1
A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -aux*workingDays[4]; A['Pr4', 'O4'] = -1

productionSigns <- rep("=", 4)
productionRHS <- rep(0,4)

A['Bk1z', 'B1'] = 1
A['Bk2z', 'B2'] = 1
A['Bk3z', 'B3'] = 1
A['Bk4z', 'B4'] = 1

invBackSigns <- rep("=", 4)
invBackRHS <- rep(0,4)

#restriccion inv4 = 2000:
A['Inv4','I4'] = 1

inv4Signs <- c("=")
inv4RHS <- c(2000)

#retriccion in1inv3:

A['I3I1','I1']= 3 ; A['I3I1', 'I3'] = -1

i3i1Signs <- c("<=")
i3i1RHS <- c(0)

# Find the optimal solution
aggregateProductionPlan <-  lp(direction = "min",
                               objective.in = objFunction,
                               const.mat = A,
                               const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns, invBackSigns,inv4Signs,i3i1Signs),
                               const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS, invBackRHS, inv4RHS , i3i1RHS ),
                               int.vec = c(4,11,18,25) #integers Wi
)  
print(paste("The total cost is: ", aggregateProductionPlan$objval))
best_sol <- aggregateProductionPlan$solution
names(best_sol) <- variablesNames
print(best_sol)
