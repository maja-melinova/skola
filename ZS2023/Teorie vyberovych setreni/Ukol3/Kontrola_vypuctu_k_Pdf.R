## Kontrola vlastních výpočtů z první části zadání
kor <- rbind(c(9, 1.5), c(1.5, 4))
cov2cor(kor)

B <- rbind(c(0, 2), c(-1, 1))

B %*% kor %*% t(B)

cov2cor(B %*% kor %*% t(B))

det(kor)
sum(diag(kor))

inv <- rbind(c(16/135, -2/45), c(-2/45, 4/15))

kor %*% inv

library(matlib)

inv(kor)

vlastni_cisla <- eigen(kor)
vlastni_cisla$values

vlastni_cisla$vectors


plot(c(0,vlastni_cisla$vectors[1,1]),c(0,vlastni_cisla$vectors[2,1]),col="red", xlab="x1",ylab="x2",xlim=c(-1.2,1.2),ylim=c(-1.2,1.2), type = "l", asp = 1)
lines(c(0,vlastni_cisla$vectors[1,2]),c(0,vlastni_cisla$vectors[2,2]),col="red")
lines(c(0,-vlastni_cisla$vectors[1,1]),c(0,-vlastni_cisla$vectors[2,1]),col="red",lty=2)
lines(c(0,-vlastni_cisla$vectors[1,2]),c(0,-vlastni_cisla$vectors[2,2]),col="red",lty=2)

vlastni_cisla$vectors %*% diag(c(sqrt(vlastni_cisla$values[1]),sqrt(vlastni_cisla$values[2]))) %*% t(vlastni_cisla$vectors)




