# # Uklid v R
rm(list = ls())
# Nacteni dat MU284: 284 svedskych municipalit
# Promenne:

#install.packages("sampling")
library(sampling)
data(MU284)
help(MU284)
POP284=MU284
names(POP284)
summary(POP284)


# HTML napoveda: 
# http://search.r-project.org/library/survey/html/00Index.html
#install.packages("survey")
library(survey)

# Prosty nahodny vyber
# Proved vyber desetiny obci a odhadni celkovy pocet obyvatel v roce 1985

N=284
n=ceiling(0.1*N)
n
# # # Pomocný vektor U=c(1,2,...,N) pro provedení výběru
U<-seq(1:N)
# # # Provedeme vyber
set.seed(123)
s=sample(U,size=n,replace=FALSE)
s
# # # Pocet obyv. v roce 1985 ve vybranych obcich
s_pnv=POP284[s,]
s_pnv
# Konecnostni nasobitel 
s_pnv$f=rep(n/N,n)
# Definice survey designu
pnv=svydesign(id=~1, fpc=~f, data=s_pnv)
# odhad poctu obyv. v roce 1985
svytotal(~P85, pnv)
sum(POP284$P85)
# cim si vysvetlujete tak velky rozdil mezi odhadem poctu obyvatel a skutecnosti?

# Stratifikovany vyber
# Vyber v kazdem regionu petinu jednotek
# Pocet obci ve stratech
table(POP284$REG)
Nh=as.vector(table(POP284$REG))
Nh
nh=ceiling(0.2*Nh)
nh
# Konecnostni nasobitel
fh=nh/Nh
# Vytvorime stratifikacni promenou
POP284$stratum=as.character(POP284$REG)
# Je stratifikace vhodna pro zpresneni odhadu celkoveho poctu obyv.v roce 1985?

# Provedeme vyber
set.seed(312)
s=stratsample(POP284$stratum,c("1"=5,"2"=10,"3"=7,"4"=8,"5"=12,"6"=9,"7"=3,"8"=6))
# # # Pocet obyv. v roce 1985 ve vybranych obcich
s_strat=POP284[sort(s),]
s_strat
s_strat=s_strat[order(s_strat$stratum),]
s_strat
# Konecnostni nasobitel 
s_strat$fstrat=rep(nh/Nh,nh)
# Definice survey designu
strat=svydesign(id=~stratum, fpc=~fstrat, data=s_strat)
# odhad poctu obyv. v roce 1985
svytotal(~P85, strat)
sum(POP284$P85)
#  odhad poctu obyv. v roce 1985 dle regionu
svyby(~P85, by=~stratum, svytotal, design=strat)

# Vyber skupin, skupina - komunita, promenna CL
# Velikost komunit
table(POP284$CL)
# Pocet skupin
N=length(table(POP284$CL))
N
Mi=as.vector(table(POP284$CL))
Mi

# Pridam promenne pro PSU a SSU
POP284$psu=as.character(POP284$CL)
POP284$ssu=as.character(POP284$LABEL)

# Vybereme petinu komunit
n=ceiling(0.2*N)
n
# Provedeme vyber petiny komunit
set.seed(954)
s_cl=sample(1:N,n,replace=F)
s_cl
# Vyberovy soubor petiny komunit
POP284_clu=POP284[POP284$CL%in%s_cl,]
POP284_clu
# Konecnostni nasobitel
POP284_clu$fclu=rep(n/N,nrow(POP284_clu))

POP284_clu

# Definice survey designu
skup=svydesign(id=~psu, fpc=~fclu, data=POP284_clu)
# odhad poctu obyv. v roce 1985
svytotal(~P85, skup)
sum(POP284$P85)

mstage(POP284,stage=list("cluster",""),varnames=list("psu"),size=list(4,rep(3,n)),method=list("srswor","srswor"))

## Najdete presne rozdeleni 10% systematickeho vyberu








