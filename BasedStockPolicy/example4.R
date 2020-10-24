#Ejercicio 4
cu<-1800-1000+500
co<-1000+180-110

lambda<-1/75

#encontrar el critical ratio
ratio<-cu/(cu+co)

#encontrar Qopt
qOpt<-qexp(p=ratio, rate=lambda)
#Question4
qOpt
Q<-qOpt

f <- function(x) {lambda*exp(-lambda*x)}

expectedUnsoldUnits <- function(x){(Q-x)*f(x)}
unsoldUnits <- integrate(expectedUnsoldUnits, lower = 0, upper = qOpt)
#Question5
unsoldUnits$value


# Expected units of short units (faltantes) y = demand
expectedShortUnits <- function(y) { (y - Q)*f(y) }
shortUnits <- integrate(expectedShortUnits,lower = qOpt, upper = Inf)
#shortUnits$value

totalCost <- cu*shortUnits$value + co*unsoldUnits$value
totalCost

revenue<-1800*qOpt
profit<-revenue-totalCost
#Question6
profit

#PAra Q=75
qOpt2<-75
expectedUnsoldUnits2 <- function(x){(qOpt2-x)*f(x)}
unsoldUnits2 <- integrate(expectedUnsoldUnits2, lower = 0, upper = qOpt2)
#unsoldUnits2$value


# Expected units of short units (faltantes) y = demand
expectedShortUnits2 <- function(y) { (y - qOpt2)*f(y) }
shortUnits2 <- integrate(expectedShortUnits2,lower = qOpt2, upper = Inf)
#shortUnits2$value

totalCost2 <- cu*shortUnits2$value + co*unsoldUnits2$value
#totalCost2

revenue2<-1800*qOpt2
profit2<-revenue2-totalCost2
#Question7
profit2

