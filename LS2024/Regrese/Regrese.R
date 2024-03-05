#(2) Cvičení 22.2.
#------------------

# 3) MNČ odhad, maticově

X <- cbind(c(1, 1, 1, 1), c(1, 5, -1, 6))
y <- c(4, 10, -1, 14)

pars <- as.data.frame(t(inv(t(X) %*% X) %*% t(X) %*% y))
#solve(crossprod(X)) %*% crossprod(X, y)
colnames(pars) <- c("b0", "b1")

plot(X[,2], y)
curve(pars$b0 + pars$b1*x, from = -2, to = 7, col = "red", add = T)





#(3) Cvičení 29.2.
#------------------

library(car)
library(rgl)
library("scatterplot3d")

Duncan <- Duncan

# ad 1) Interaktivní 3D graf
scatter3d(prestige ~ income + education, data = Duncan, surface = FALSE)
scatterplot3d(Duncan$prestige ~ Duncan$income + Duncan$education)

# ad 2) Zakreslení regresní funkce do grafu
scatter3d(prestige ~ income + education, data = Duncan, id=list (n=5))

# ad 3) Bodový diagramy a histogram
scatterplotMatrix(~ prestige + education + income, id=list (n=3),
                  data=Duncan)
with (Duncan, hist (prestige))

# ad 4) MNČ odhady reg. parametrů
fit = lm(prestige ~ income + education,
         data = Duncan)
summary(fit)
coef(fit)

# ad 5) Vyrovnaní hodnoty a rezidua
fitted(fit)
resid(fit)

# ad 6) Reziduální rozptyl
summary(fit)$sigma^2

# ad 7) Odhady chyb regr. parametrů
coef(summary(fit))

# Maticové identity ---
# a) Regresní matice a její rozměry
X = model.matrix(fit)
head(X)
dim(X)

# b) Odhady regr. parametrů
y = Duncan[ ,4]
solve(t(X)%*%X) %*% t(X)%*%y
coef(fit)

# c) Matice H a M. Diagonální prvky H
H = X %*% solve(t(X)%*%X) %*% t(X)
dim(H)
M = diag(nrow(X)) - H
dim(M)

diag(H)
hatvalues(fit)

# d) Vektor vyrovnaných hodnot
H %*% y
fitted(fit)

# e) Vektor reziduí
M %*% y
resid(fit)

# f) Kovarianční matice vektoru b
s2e = sum(resid(fit)^2)/(nrow(X) - 3) 
s2e * solve(t(X)%*%X)
vcov(fit)

sqrt(diag(vcov(fit)))
coef(summary(fit))[ ,2]

