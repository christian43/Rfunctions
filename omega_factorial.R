# omega_factorial(50,3,3,37332,402589,47894,1820987)
omega_factorial <- function(n, a, b, SSa, SSb, SSab, SSr){
MSa <- SSa/(a-1)
MSb <- SSb/(b-1)
MSab <- SSab/((a-1)*(b-1))
MSr <- SSr/(a*b*(n-1))
varA <- ((a-1)*(MSa-MSr))/(n*a*b)
varB <- ((b-1)*(MSb-MSr))/(n*a*b)
varAB <- ((a-1)*(b-1)*(MSab-MSr))/(n*a*b)
varTotal <- varA + varB+ varAB + MSr
print(paste("Omega-Squared A: ", varA/varTotal))
print(paste("Omega-Squared B: ", varB/varTotal))
print(paste("Omega-Squared AB: ", varAB/varTotal))
}


