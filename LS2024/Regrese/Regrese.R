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




