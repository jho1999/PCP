require(lpSolve)
#lpSolveAPI --> no lo vamos a usar

numberVariables = 39
numberConstraints = 21

variablesNames = c('W0','I0','B0', 
                   'W1','H1','F1','I1','B1','P1',
                   'W2','H2','F2','I2','B2','P2',
                   'W3','H3','F3','I3','B3','P3',
                   'W4','H4','F4','I4','B4','P4',
                   'W5','H5','F5','I5','B5','P5',
                   'W6','H6','F6','I6','B6','P6')

constrainstNames = c('InitW', 'InitIn', 'InitBk',
                     'Wo1','Wo2', 'Wo3','Wo4','Wo5','Wo6',
                     'In1', 'In2','In3','In4','In5','In6',
                     'Pr1','Pr2','Pr3','Pr4','Pr5','Pr6')


# Declara la función objetivo
objFunction <- c(0,0,0,
                 15*8*21,450,600,5,15,0, #january
                 15*8*20,450,600,5,15,0, #febreuary
                 15*8*23,450,600,5,15,0,
                 15*8*21,450,600,5,15,0,
                 15*8*22,450,600,5,15,0,
                 15*8*22,450,600,5,15,0) #June


A <- matrix(0, nrow = numberConstraints, ncol = numberVariables, 
               dimnames = list(constrainstNames, variablesNames)
           )




A['InitW', 'W0'] = 1
A['InitIn', 'I0'] = 1
A['InitBk', 'B0'] = 1

intialSigns <- rep("=", 3)

initialsRHS <- c(35,
                 0,
                 0)

A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
A['Wo4', 'W4'] = 1; A['Wo4', 'W3'] = -1; A['Wo4', 'H4'] = -1; A['Wo4', 'F4'] = 1;
A['Wo5', 'W5'] = 1; A['Wo5', 'W4'] = -1; A['Wo5', 'H5'] = -1; A['Wo5', 'F5'] = 1;
A['Wo6', 'W6'] = 1; A['Wo6', 'W5'] = -1; A['Wo6', 'H6'] = -1; A['Wo6', 'F6'] = 1;

workersSigns <- rep("=", 6)

workersRHS <- rep(0,6)

A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1;
A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1;
A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1;
A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1;
A['In5', 'I5'] = 1; A['In5', 'B5'] = -1; A['In5', 'I4'] = -1; A['In5', 'B4'] = 1; A['In5', 'P5'] = -1;
A['In6', 'I6'] = 1; A['In6', 'B6'] = -1; A['In6', 'I5'] = -1; A['In6', 'B5'] = 1; A['In6', 'P6'] = -1;

inventorySigns <- rep("=", 6)

inventoryRHS <- c(-2760,
                  -3320,
                  -3970,
                  -3540,
                  -3180,
                  -2900)

A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -(41383/(260*40))*21;
A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -(41383/(260*40))*20;
A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -(41383/(260*40))*23;
A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -(41383/(260*40))*21;
A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -(41383/(260*40))*22;
A['Pr6', 'P6'] = 1; A['Pr6', 'W6'] = -(41383/(260*40))*22;

productionSigns <- rep("=", 6)
productionRHS <- rep(0,6)


# Find the optimal solution
aggregateProductionPlan <-  lp(direction = "min",
                               objective.in = objFunction,
                               const.mat = A,
                               const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns),
                               const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS),
                               )

#Print solution
print(paste("The total cost is: ", aggregateProductionPlan$objval))
best_sol <- aggregateProductionPlan$solution
names(best_sol) <- variablesNames
print(best_sol)





