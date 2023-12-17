library(EnvStats)

##----
## 3.
##----

hustotyPareto <- function(x, r){
  vysledek <- NA
  
  if(r == 1){
    #minimum
    vysledek <- ifelse(x < 1, 0, 20/(x^21))
  }
  if(r == 3){
    #median
    #https://stats.stackexchange.com/questions/243795/what-is-the-mean-and-variance-of-the-median-of-a-set-of-i-i-d-normal-random-vari
    vysledek <- ifelse(x < 1, 0, choose(4, 2) * (1 - (1/x)^4)^2 * 4/(x^5) * (1-(1-(1/x)^4))^2)
  }
  if(r == 5){
    #maximum
    vysledek <- ifelse(x < 1, 0, 5 * (4/(x^5)) * ((1/x)^4)^2)
  }
  
  return(vysledek)
}

curve(dpareto(x, location = 1, shape = 4), from = 0.8, to = 2, ylim = c(0, 20.5), ylab = ("PDF"))
curve(hustotyPareto(x, 1), from = 0.8, to = 2, col = "red", add = T)
curve(hustotyPareto(x, 3), from = 0.8, to = 2, col = "green", add = T)
curve(hustotyPareto(x, 5), from = 0.8, to = 2, col = "blue", add = T)
legend(1.3, 20, legend=c("Paretovo rozdělení", "Rozdělení minima", "Rozdělení mediánu", "Rozdělení maxima"),
       col=c("black", "red", "green", "blue"), lty=1, cex=0.7)

##----
## 4.
##----

vybery <- as.data.frame(matrix(ncol = 0, nrow = 50))
set.seed(42) #zafixuji pouze generování prvního výběru, abych mohla interpretovat test
vybery$prvni <- sort(rpareto(50, location = 1, shape = 4))
vybery$druhy <- sort(rpareto(50, location = 1, shape = 4))
vybery$treti <- sort(rpareto(50, location = 1, shape = 4))

#Distribuční funkce
curve(ppareto(x, location = 1, shape = 4), from = 0.8, to = 2.5, ylab = "CDF")
lines(ecdf(vybery$prvni), main = "", col = "darkred", lwd = 1.5, do.points=F)




#Kvantolová funkce
lin_kvantily <- as.data.frame(matrix(nrow = 1000, ncol = 0))
lin_kvantily$x <- seq(from = 0, by = 1/1000)[2:1001]
lin_kvantily$kvantil <- quantile(vybery$druhy, probs = lin_kvantily$x)
lin_kvantily$kvantil2 <- quantile(vybery$druhy, probs = lin_kvantily$x, type = 2)

curve(qpareto(x, location = 1, shape = 4), from = 0, to = 1, ylab = "Kvantilová funkce", ylim = c(1, 3.7))
lines(x = lin_kvantily$x, y = lin_kvantily$kvantil, type = "l", col = "darkred", lwd = 1.5)
lines(x = lin_kvantily$x, y = lin_kvantily$kvantil2, type = "l", col = "darkgreen", lwd = 1.5)

#Histogram, teoretická hustota a jádrový odhad hustoty
hist(vybery$treti, freq = F, xlim = c(0.8, 3), ylim = c(0, 4), xlab = "x", main = "")
curve(dpareto(x, location = 1, shape = 4), from = 0.8, to = 3, add = T, lwd = 2)
lines(density(vybery$treti, adjust = 1.5), lwd = 1.5, col = "darkred")

#------------------------
#Kolmogoruv Smirnuv test

#H0: data pochází z Parotova rozdělení
#H1: data nepochází z Parotova rozdělení

test <- ks.test(vybery$prvni, "ppareto", location = 1, shape = 4)
test$p.value < 0.05

#-> nepravda, nezamítáme nulovou hypotézu
# můžeme říci, že data pochází z Paretova rozdělení

#-----------------------
#Charakteristiky výběru

winsorizovany_prumer <- function(data, n, k){
  vysledek <- ((k+1)*data[k+1] + sum(data[(k+2):(n-k-1)]) + (k+1)*data[n-k])/n
  return(vysledek)
}

trimean <- function(data){
  vysledek <- (quantile(data, probs = 0.25) + 2*median(data) + quantile(data, probs = 0.75))/4
  return(vysledek)
}

stredni_abs_odchylka <- function(data, n){
  vysledek <- 1/n * sum(abs(data - mean(data)))
  return(vysledek)
}

giniho_koeficient <- function(data, n){
  absZmeny <- c()
  for(i in 1:(n-1)){
    absZmeny[i] <- abs(data[i] - data[i+1])
  }
  
  vysledek <- 1/choose(n, 2) * 1/n * sum(absZmeny)
  return(vysledek)
}

charakteristiky <- as.data.frame(matrix(nrow = 10, ncol = 3))
rownames(charakteristiky) <- c("Aritmetický průměr", "Useknutý průměr", "Winsorizovaný průměr", "Medián", "Trimean", "Výběrový rozptyl", "Výběrová sm. odch.","Střední abs. odch", "MAD", "Giniho koeficient")
colnames(charakteristiky) <- c("Výběr 1", "Výběr 2", "Výběr 3")

charakteristiky[1,] <- apply(vybery, 2, mean)
charakteristiky[2,] <- apply(vybery[6:45,], 2, mean)
charakteristiky[3,] <- apply(vybery, 2, winsorizovany_prumer, n = 50, k = 5)
charakteristiky[4,] <- apply(vybery, 2, median)
charakteristiky[5,] <- apply(vybery, 2, trimean)
charakteristiky[6,] <- apply(vybery, 2, var)
charakteristiky[7,] <- apply(vybery, 2, sd)
charakteristiky[8,] <- apply(vybery, 2, stredni_abs_odchylka, n = 50)
charakteristiky[9,] <- apply(vybery, 2, mad)
charakteristiky[10,] <- apply(vybery, 2, giniho_koeficient, n = 50)

charakteristiky

