##------------------------------------------------------------------------------
## Optimalizace
##------------------------------------------------------------------------------

#install.packages("GA")
#install.packages("pso")
#install.packages("matlib")

#__________
# Ukol 1.1

data <- c(168, 179, 162, 169, 158, 168, 169, 169, 181, 160)
n <- as.numeric(length(data))

#MLE odhady
stredniHodnotaMLE <- (1/n) * sum(data)
rozptylMLE <- (1/n) * sum((data - stredniHodnotaMLE)^2)

#Sestaveni verohodnostni funkce a hledani jejiho maxima
normLikelihoodF <- function(data, param){
  mean <-  param[1]
  sd <- param[2]
  
  ll <- (-(n/2)*log(pi) -(n/2)*log(sd^2) -(1/(2*sd^2))*sum((data-mean)^2))
  return(-ll)
}


MLE <- optim(par=c(mean = 168,sd = 7), normLikelihoodF, d = data)
MLE$par

#__________
# Ukol 1.2
library(GA)
library(pso)

fce <- function(par){
  x <- par[1]
  y <- par[2]
  
  return((x-3.12)^2 + (y-2.72)^2 + sin(3*x + 1.41) + sin(4*y - 1.73))
}

fceGA <- function(x, y){
  return((x-3.12)^2 + (y-2.72)^2 + sin(3*x + 1.41) + sin(4*y - 1.73))
}

optimalizace <- function(method){
  result <- c()
  
  #Optimalizace pomoci funkce OPTIM
  if(method == "optim"){
    optOPTIM <-  optim(par = c(1, 1), fce, method = "L-BFGS-B", lower = 0, upper = 5)
    result <- c(optOPTIM$par[1], optOPTIM$par[2], optOPTIM$value)
  }
  
  #Optimalizace pomoci funkce GA
  if(method == "ga"){
    optGA <- ga(type = "real-valued", fitness =  function(x) -fceGA(x[1], x[2]), lower = c(0, 0), upper = c(5, 5), popSize = 50, maxiter = 1000, run = 100)
    result <- c(optGA@solution[1], optGA@solution[2], fceGA(optGA@solution[1], optGA@solution[2]))
  }
  
  #Optimalizace pomoci funkce PSO
  if(method == "pso"){
    vysledkyPSO <-  data.frame(matrix(nrow = 1000, ncol = 3))
    
    for(i in 1:1000){
      optPSO <- psoptim(rep(NA, 2), fce, lower = 0, upper = 5, control=list(abstol=1e-8))
      vysledkyPSO[i, 1] <- optPSO$par[1]
      vysledkyPSO[i, 2] <- optPSO$par[2]
      vysledkyPSO[i, 3] <- fce(c(optPSO$par[1], optPSO$par[2]))
    }
    
    result <- c(vysledkyPSO[which.min(vysledkyPSO[,3]),][1], vysledkyPSO[which.min(vysledkyPSO[,3]),][2], vysledkyPSO[which.min(vysledkyPSO[,3]),][3])
  }
  
  return(result)
}

#Vypsani vysledku
vysledky <- data.frame(matrix(nrow = 3, ncol = 3))

vysledky[1,] <- optimalizace("optim")
vysledky[2,] <- optimalizace("ga")
vysledky[3,] <- optimalizace("pso")

rownames(vysledky) <- c("OPTIM", "GA", "PSO")
colnames(vysledky) <- c("x", "y", "f(x, y)")

vysledky

#__________
# Ukol 1.3

library(matlib)

#https://www.kaggle.com/datasets/yasserh/student-marks-dataset
data_SM <- read.csv("Student_Marks.csv")

x_SM_1 <- as.data.frame(matrix(nrow = 100, ncol = 3))
x_SM_1[,1] <- 1
x_SM_1[,2:3] <- data_SM[,1:2]
x_SM_1 <- as.matrix(x_SM_1)
y_SM <- data_SM[,3]

betaEst <- inv(t(x_SM_1) %*% x_SM_1) %*% t(x_SM_1) %*% y_SM
betaEst <- t(betaEst)
betaEst <- setNames(c(betaEst[1], betaEst[2], betaEst[3]), c("b0", "b1", "b2"))

fceLAD <- function(x, y, param){
  b0 <- param[1]
  b1 <- param[2]
  b2 <- param[3]
  
  x1 <- x[,1]
  x2 <- x[,2]
  
  return(sum(abs(y - b0 - b1*x1 - b2*x2)))
}

LAD <- optim(par=c(b0 = 0, b1 = 0, b2 = 0), fceLAD, x = x_SM_1[,2:3], y = y_SM)

parametry_odh <- data.frame(matrix(nrow = 0, ncol = 3))
parametry_odh[1,] <- betaEst
parametry_odh[2,] <- LAD$par

rownames(parametry_odh) <- c("OLS", "LAD")
colnames(parametry_odh) <- c("b0", "b1", "b2")

parametry_odh

##------------------------------------------------------------------------------
## Feature Engineering
##------------------------------------------------------------------------------

#__________
# Ukol 2.1

library(ggplot2)

dataPom <- read.csv("ARAD-uvery.csv", sep = ";")
dataPom[,5] <- gsub(',', '.', dataPom[,5])

dataARAD <- as.data.frame(matrix(ncol = 0, nrow = nrow(dataPom)))

dataARAD$obdobi <- as.Date(dataPom[,1], "%Y-%m-%d")
dataARAD$uvery <- as.numeric(dataPom[,5])
dataARAD$uveryTis <- dataARAD$uvery/1000

dataARAD <- dataARAD[order(dataARAD[,1]),]

ggplot(dataARAD, aes(x = obdobi, y = uveryTis)) +
  geom_line() +
  xlab("Období") + ylab("Objem úvěrů v tis. ks") + ggtitle("Úvěry poskytnuté nefinančním podnikům")

#odstraneni trendu
dataARAD$t <- seq(1:nrow(dataARAD))
lm <- lm(uveryTis ~ t, dataARAD)
dataARAD$uveryTis_bezTrendu <- lm$residuals + lm$coefficients[1]

ggplot(dataARAD, aes(x = obdobi, y = uveryTis_bezTrendu)) +
  geom_line() +
  xlab("Období") + ylab("Objem úvěrů v tis. ks") + ggtitle("Úvěry poskytnuté nefinančním podnikům očištěné o lineární trend")








