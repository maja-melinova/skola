# stazeni dat
hdp <- c(read.csv2("hdp.csv")[,-c(1)])
hdpln <- log(hdp)

plot.ts(hdpln)

# HP filtr --------------------------------------------------------------------
nObs <- length(hdpln)
l1 <- 1600
I <- diag(nObs)
D <- diff(I, lag=1, d= 2)

bigX <- rbind(I,sqrt(l1)*D)
bigy <- c(hdpln, sqrt(l1)*rep(0, nObs-2))

# Var.1 ---- velke OLS
hp.trend <- solve(t(bigX)%*%bigX)%*%t(bigX)%*%bigy
# Var.2 ---- rozepsano
hp.trend <- solve(I + l1*t(D)%*%D)%*%hdpln
hp.gap <- hdpln - hp.trend

lines(hp.trend, col = "red")
plot(hp.gap, type = "l")
abline(0,0)
#------------------------------------------------------------------------------

hp <- function(data, type){
  nObs <- length(data)
  l1 <- 1600
  I <- diag(nObs)
  D <- diff(I, lag=1, d= 2)
  
  bigX <- rbind(I,sqrt(l1)*D)
  bigy <- c(data, sqrt(l1)*rep(0, nObs-2))
  
  hp.trend <- solve(I + l1*t(D)%*%D)%*%data
  hp.gap <- data - hp.trend
  if(type == "trend"){
    
    return(hp.trend)
  }
  if(type == "gap"){
    return(hp.gap)
  }
}


ukol <- c()

for(i in 1:16){
  data_ukol <- hdpln[1:(i+89)]
  hpData_ukol <- hp(data_ukol, "trend")
  ukol[i] <- hpData_ukol[90]
}

plot(ukol, type = "l")

# pridani omezeni na mezirocni rust -------------------------------------------
l2 <- 15
rr <- c(rep(0,nObs-5),-1,0,0,0,1)
bigX.rr <- rbind(I, sqrt(l1)*D, sqrt(l2)*rr)
bigy.rr <- c(hdpln, sqrt(l1)*rep(0, nObs-2), sqrt(l2)*0.025)
# Var.1 --- velke OLS
hp.trend.rr <- solve(t(bigX.rr)%*%bigX.rr)%*%t(bigX.rr)%*%bigy.rr
# Var.2 ---> zkuste rozepsat sami
#------------------------------------------------------------------------------



# "predpovidani/projekce" hp.trendu s predchodem do SS ----------------------------------
l3 <- 15 
nAhead <- 8
I.f <- cbind(diag(nObs), matrix(0,nObs,nAhead))
D.f <- diff(diag(nObs+nAhead), lag = 1, d = 2)
rrf <- c(rep(0, nObs+nAhead-5),-1,0,0,0,1)
bigX.f <- rbind(I.f, sqrt(l1)*D.f, sqrt(l3)*rrf)
bigy.f <- c(hdpln, rep(0,nObs+nAhead-2), sqrt(l3)*0.025)
hp.trend.f <- solve(t(bigX.f)%*%bigX.f)%*%t(bigX.f)%*%bigy.f     # Var.1
#---------------------------------------------------------------------------------------




# mezera vystupu-----------------------------------------------------------------
l4 <- 0.001
nLags <- 4
gap.dt <- embed(hp.gap, nLags + 1)
y <- gap.dt[,1]
X <- gap.dt[,2:(nLags+1)]
pars.ols <- solve(t(X)%*%X)%*%t(X)%*%y
# Var.1: standardni hrebenova regrese
pars.ridge1 <- solve(t(X)%*%X + l4*diag(nLags))%*%t(X)%*%y
pars.ridge2 <- solve(t(X)%*%X + l4*diag(nLags))%*%(t(X)%*%y + l4*c(1.5,-0.6,0,0))
# --------------------------------------------------------------------------------




# vazene OLS jako LASSO?? - IRLS -------------------------------------------------
W <- diag(nLags)
for (i in 1:100)
{
pars.lasso <-  solve(t(X)%*%X + l4*W)%*%t(X)%*%y
ei <- abs(0 - pars.lasso)
W <- diag(c(1/pmax(abs(ei),0.0000000005)))
}
# optimalizujte zakladni kod vyse 
# (pozn. tento typ kodu/snippetu je mozne pouzit i u ruznych hodnotach lambda pro jednotlive promenne ci pozorovani)
#-------------------------------------------------------------------------------------------------------------
  
  

