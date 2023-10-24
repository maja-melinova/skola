# # # 4ST414, 19/10/2022, Regresní přímka

# # # Jak odhadnout parametry regresní přímky Y=A+BX,
# # # Y je vysvětlovaná proměnná, X je závislá proměnná, epsilon je náhodná chyba 
# # # Máme n pozorování: (X_i,Y_i): i=1,...,n.


# # # Př. Vrabec a Čermák z 4. přednášky, populace N=5 pozorování
x<-c(3,5,8,8,12)
y=c(0,0,2,3,3)
# # # Vytvoříme dataframe PrVC (Priklad Vrabec, Cermak)
PrVC<-data.frame(x,y)

print(PrVC)
# # # Popisné statistiky
summary(PrVC)
# # # Korelační matice
cor(PrVC)
# # # Jak získat jednotlivé proměnné dataframe
PrVC$x
PrVC$y
# # # Korelační koeficient proměnných x a y
cor(PrVC$x,PrVC$y)

# # # Odhad populačních parametrů A,B z dataframe

fit1<-lm(y~x,data=PrVC)
summary(fit1)
names(fit1)
# # # Jak získat hodnoty regresních populačních koeficientů A,B
A<-summary(fit1)$coefficients[1]
B<-summary(fit1)$coefficients[2]
A
B



# # # Vykreslit data s populační regresní přímkou
plot(PrVC$x,PrVC$y,xlab="x",ylab="y",pch=16,col="blue")
abline(fit1,col="red")

# # # Odhad populačních parametrů A, B z dvou vektorů X,Y
X<-PrVC$x
Y<-PrVC$y
# # # Populační průměry:
mean(X)
mean(Y)
# # # Populační korelační koeficient
cor(X,Y)
# # # Populační regresní koeficienty:
fit2<-lm(Y~X)
summary(fit2)

A<-summary(fit2)$coefficients[1]
B<-summary(fit2)$coefficients[2]
A
B
# # # Vykreslit data s populační regresní přímkou
plot(X,Y,xlab="x",ylab="y",pch=16,col="blue")
abline(fit2,col="red")


# # # Jak odhadnout parametry regresní přímky Y=BX, (bez absolutního členu, přímá úměrnost, úměrová regrese)
# # # Y je vysvětlovaná proměnná, X je závislá proměnná
# # # Máme n pozorování: (X_i,Y_i): i=1,...,n.
# # # to samé dokáže lm(y~0+x)
fit3<-lm(y~x-1)
summary(fit3)
names(fit3)
# # # Jak získat hodnoty regresních populačních koeficientů
B<-summary(fit3)$coefficients[1]
B
plot(x,y,xlab="x",ylab="y",pch=16,col="blue")
abline(fit1,col="red")
abline(fit3,col="green")



