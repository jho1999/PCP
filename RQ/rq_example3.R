###Example 3

lambda = 25000
sigma = 36
tau = 1*(1/52)
lambdaTau = lambda*tau
sigmaTau = sigma*sqrt(tau)

k= 50
c = 1000
i = .25
pi = 4

h = i*c
lostFunctionNormal <- function(x){ exp(-(x^2)/2)/(sqrt(2*pi)) - x*(1-pnorm(x))}
  
  #j=0
  nR = 0 
  Q0 = sqrt((2*lambda*(k+nR))/h)
  R0 = qnorm(1- ((Q0*h)/(pi*lambda)), lambdaTau, sigmaTau)
  nR0 = lostFunctionNormal(1- ((Q0*h)/(pi*lambda)))*sigmaTau
  
  #j=1
  Q1 = sqrt((2*lambda*(k+nR0))/h)
  R1 = qnorm(1- ((Q1*h)/(pi*lambda)), lambdaTau, sigmaTau)
  nR1 = lostFunctionNormal(1- ((Q1*h)/(pi*lambda)))*sigmaTau
  
  #j=2
  Q2 = sqrt((2*lambda*(k+nR1))/h)
  R2 = qnorm(1- ((Q2*h)/(pi*lambda)), lambdaTau, sigmaTau)
  nR2 = lostFunctionNormal(1- ((Q2*h)/(pi*lambda)))*sigmaTau