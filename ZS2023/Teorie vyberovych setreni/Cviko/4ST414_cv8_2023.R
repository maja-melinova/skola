# # Uklid v R
rm(list = ls());

# Pouzivana R library sampling

install.packages("sampling")
library(sampling)

# Pr. 1: 2 x hazeme pravidelnou desetistennou kostkou
# a) Urcete zakladni soubor
# b) Jedna se o vyber se stejnymi nebo ruznymi pravdepodobnostmi?
# c) Jedna o vyber bez vraceni nebo s vracenim?
# d) Urcete vektor pravdepodobnosti pi, ze dane cislo padne v i-tem tahu
# e) Urcete vektor pravdepodobnost phi, ze ve 2 tazich padne aspon jedno cislo.
# f) Urcete nejmensi rozsah vyber, tj. pocet hodu, aby pravdepodnost zahrnuti do vyberu dosahla aspon 0.9.

# Pr. 2: 3 x hazene nepravidelnou desetistennou kostkou.
# Pravdepodobnost, ze padne i, i=1,2,...,10 je primo umerna velikosti cisla i
# a) Urcete zakladni soubor
# b) Jedna se o vyber se stejnymi nebo ruznymi pravdepodobnostmi?
# c) Jedna o vyber bez vraceni nebo s vracenim?
# d) Urcete vektor pravdepodobnosti pi, ze dane cislo padne v i-tem tahu
# e) Urcete vektor pravdepodobnost phi, ze ve 2 tazich padne aspon jedno cislo.

# Pr. 3: Populace U ma 100 jednotek, a to 1,2,...,99,10000
# Urcete pravdpodonosti vyberu v danem tahu umerne velikosti pro 10% vyber.
# Proc nize uvedeny vzorec nefunguje?

U=c(1:99,10000)
N=length(U)
N
n=0.1*N
n
pi=n*U/sum(U)
pi
summary(pi)
max(pi)

# funkce inclusionprobabilities
pi1=inclusionprobabilities(U,n)
pi1
sum(pi1)

# Urcete vektor pravdepodobnosti zahrnuti do vyberu o rozsahu n

# Provedte je vyber s timto vyberovym planem.

help(sample)
sample(U,n,replace=F,pi1)


# Pr. 4, provadime prosty nahodny vyber s vracenim, vybereme vzdy polovinu jednotek
# Urcete pravdepodobnost zahrnuti do vyberu pro obecne N a n
# Jaka je tato pravdepodobnost, pokud je setrena populace velmi velka.

# Pr. 5 Hansen-Hurvitzuv odhad vs. Horvitz-Thompsonuv
# Data belgianmunicalities
# Tot03: pocet obyvatel v roce 2003
# Tot04: pocet obyvatel v roce 2004
# Provedeme vyber n=50 jednotek, odhadneme prumerny pocet obyvatel minicipality v roce 2004
# Srovname: prosty nahodny vyber s vraceni a bez vraceni s proporcionalnim vyberem dle poctu obyvatel v roce 2003
# Spoctete a srovnejte pravdepodobnosti zahrnuti do vyberu
# Ktery odhad byste preferovali z hlediska presnosti.
Pop3=

# 




