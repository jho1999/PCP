#ejercicio 3 clase 13 oct
#cu<-5-3.5
#co<-3.5-2.5

#lambda-U(48,72)
#a<-48
#b<-72

#encontrar el critical ratio
#ratio<-cu/(cu+co)

#encontrar Qopt
#qOpt<-qunif(p=ratio, min=a, max=b)


# Determine the Q* with uniform distribution -------->>>>>>
cu <- 5-3.5
co <- 3.5-2.5

criticalRatio =cu/(co+cu)

a <- 48
b <- 72

Qopt = a + criticalRatio*(b-a) # Buscamos en su libro de estadística


Q = Qopt
# Expected unsold units (sobrantes) x = demand
expectedUnsoldUnits <- function(x) { (Q - x)*(1/(b-a)) }
unsoldUnits <- integrate(expectedUnsoldUnits, lower = a, upper = Qopt)


# Expected units of short units (faltantes) y = demand
expectedShortUnits <- function(y) { (y - Q)*(1/(b-a)) }
shortUnits <- integrate(expectedShortUnits,lower = Qopt, upper = b)

totalCost <- cu*shortUnits$value + co*unsoldUnits$value
totalCost

print(paste0("Average overage units: ", unsoldUnits$value))
print(paste0("Average underage units: ", shortUnits$value))

print(paste0("Average overage cost: ", co*unsoldUnits$value))
print(paste0("Average underage cost: ", cu*shortUnits$value))












#--------------------------------------
# Compute all costs
#--------------------------------------
datalist = list()
index <- 1
for(Q in a:b){
  
  expectedUnsoldUnits <- function(x) { (Q - x)*(1/(b-a)) }
  unsoldUnits <- integrate(expectedUnsoldUnits,a,Q)
  
  # Expected units of short units
  expectedShortUnits <- function(y) { (y - Q)*(1/(b-a)) }
  shortUnits <- integrate(expectedShortUnits,lower = Q, upper = b)
  
  dat <- data.frame( Q = Q, 
                     totalCostO = co*unsoldUnits$value, 
                     totalCostU = cu*shortUnits$value,
                     TotalCost = cu*shortUnits$value + co*unsoldUnits$value)
  datalist[[index]] <- dat
  index <- index + 1
}
TotalCost = do.call(rbind, datalist)


#--------------------------------------
# Graph
#--------------------------------------
library(ggplot2)

p <- ggplot() + 
  geom_line(data = TotalCost, aes(x = Q, y = totalCostO), color = "blue") +
  geom_point(data = TotalCost, aes(x = Q, y = totalCostO), color = "blue") +
  geom_line(data = TotalCost, aes(x = Q, y = totalCostU), color = "red") +
  geom_point(data = TotalCost, aes(x = Q, y = totalCostU), color = "red") +
  geom_line(data = TotalCost, aes(x = Q, y = TotalCost), color = "black") +
  geom_point(data = TotalCost, aes(x = Q, y = TotalCost), color = "black") +
  scale_x_continuous(breaks=TotalCost$Q) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab('Q') +
  ylab('Cost ($)')

print(p)
