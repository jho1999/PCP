require(lpSolve)
#lpSolveAPI --> no lo vamos a usar

numberVariables = 4*6+3 
numberConstraints = 4*4+3

variablesNames = c('W0','I0','B0', 
                   'W1','H1','F1','I1','B1','P1',#oct
                   'W2','H2','F2','I2','B2','P2',#nov
                   'W3','H3','F3','I3','B3','P3',#dic
                   'W4','H4','F4','I4','B4','P4')#ene

constrainstNames = c('InitW', 'InitIn', 'InitBk',
                     'Wo1','Wo2', 'Wo3','Wo4',
                     'In1', 'In2','In3','In4',
                     'Pr1','Pr2','Pr3','Pr4',
                     'Bk1z','Bk2z','Bk3z','Bk4z',
                     'In4Z', 'Inv12') #agregamos las de no B y la ultima de inv


# Declara la función objetivo
# W H F I B P
objFunction <- c(0,0,0,
                 500,6000,300,600,50,75, #1st Qrt
                 500,6000,300,600,50,75, #2nd Qrt
                 500,6000,300,600,50,75, #3rd Qrt
                 500,6000,300,600,50,75) #4rd Qrt


A <- matrix(0, nrow = numberConstraints, ncol = numberVariables, 
               dimnames = list(constrainstNames, variablesNames)
           )




A['InitW', 'W0'] = 1
A['InitIn', 'I0'] = 1
A['InitBk', 'B0'] = 1

intialSigns <- rep("=", 3)

initialsRHS <- c(10, #dicen que en oct empiezan 10 workers
                 0,
                 50) #B0=50

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

inventoryRHS <- c(-831,
                  -838,
                  -845,
                  -852) #el problema pide que la ultima no haya 

aux<-ceiling(9342/(254*10))
A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -aux*24;
A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -aux*26;
A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -aux*24;
A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -aux*20;


productionSigns <- rep("<=", 4)
productionRHS <- rep(0,4)



A['Bk1z', 'B1'] = 1
A['Bk2z', 'B2'] = 1
A['Bk3z', 'B3'] = 1
A['Bk4z', 'B4'] = 1

invBackSigns <- rep("=", 4)
invBackRHS <- c(rep(0,4))

A['In4Z', 'I4'] = 1
invEneroCero <- "="
invEneroRHS <- 0




#el problema pide que la ultima no haya 

# Find the optimal solution
aggregateProductionPlan <-  lp(direction = "min",
                               objective.in = objFunction,
                               const.mat = A,
                               const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns,invBackSigns,invEnecero),
                               const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS,invBackRHS,invEneroRHS),
                               )

#Print solution
print(paste("The total cost is: ", aggregateProductionPlan$objval))
best_sol <- aggregateProductionPlan$solution
names(best_sol) <- variablesNames
print(best_sol)





