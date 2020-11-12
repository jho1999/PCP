##Preguntas 4-6 del quiz 5

cu <- 8-2
co <- 2+1

criticalRatio =cu/(co+cu)

a <- 200
b <- 400

Qopt<-qunif(criticalRatio, a,b) #nos dice que es con una uniforme.


Q <- Qopt
# Expected unsold units (sobrantes) x = demand
expectedUnsoldUnits <- function(x) { (Q-x)*(1/(b-a)) }
unsoldUnits <- integrate(expectedUnsoldUnits, lower = a, upper = Qopt)



# Expected units of short units (faltantes) y = demand
expectedShortUnits <- function(y) { (y - Q)*(1/(b-a)) }
shortUnits <- integrate(expectedShortUnits,lower = Qopt, upper = b)

#auxiliares
totalCost <- cu*shortUnits$value + co*unsoldUnits$value
totalEarning <- Qopt * 8

expectedProfit <- totalEarning - totalCost



####Si Q =300
Q1 = 300
# Expected unsold units (sobrantes) x = demand
expectedUnsoldUnits1 <- function(x) { (Q1-x)*(1/(b-a)) }
unsoldUnits1 <- integrate(expectedUnsoldUnits1, lower = a, upper = Q1)


# Expected units of short units (faltantes) y = demand
expectedShortUnits1 <- function(y) { (y - Q1)*(1/(b-a)) }
shortUnits1 <- integrate(expectedShortUnits1,lower = Q1, upper = b)

totalCost1 <- cu*shortUnits1$value + co*unsoldUnits1$value


totalEarning1 <- Q1 * 8


expectedProfit1 <- totalEarning1 - totalCost1


#Respuestas
paste0("Pregunta 5_1: ", ceiling(Qopt))
paste0("Pregunta 5_2: ", ceiling(shortUnits$value)) 
paste0("Pregunta 5_3: ", ceiling(unsoldUnits$value)) 
paste0("Pregunta 5_4: ",ceiling(expectedProfit)) #pregunta 5_4
paste0("Pregunta 6: ", expectedProfit1)