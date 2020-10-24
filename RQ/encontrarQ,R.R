
a <- 150
b <- 350

lambda <- 0.5*(a+b)

c <- 360#$/un
K <- 20#$
tau <- 1#semana
i <- .2/12 #%/mes
pi <- 5#$/unidades

h <- i*c




nRp<-0
Q <- sqrt((2*lambda*(K+pi*nRp))/h)
R <- a + (1-((Q*h)/(pi*lambda)) )*(b-a)
expectedShortUnits <- integrate(function(D){ (D-R)*(1/(b-a)) },
                                lower=R, upper=b)
nRp <- expectedShortUnits$value
i<-0

eps<-2.2204e-16
#intento de un while
repeat{
  i<-i+1
  Qp<-Q
  Rp<-R
  Q <- sqrt((2*lambda*(K+pi*nRp))/h)
  R <- a + (1-((Q*h)/(pi*lambda)) )*(b-a)
  expectedShortUnits <- integrate(function(D){ (D-R)*(1/(b-a)) },
                                  lower=R, upper=b)
  nRp <- expectedShortUnits$value
  if (abs(R-Rp)<eps || abs(Q-Qp)<eps || i>1000){break}
}
