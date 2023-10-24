# # # 4ST414, 24/10/2023, Poměrový odhad - průměr

# # # Očekáváme závislost Y=RX (přímá úměrnost, tzv. úměrová regrese),
# # # Y je vysvětlovaná proměnná, X je závislá proměnná
# # # Máme n pozorování: (X_i,Y_i): i=1,...,n.


# # # Př. Vrabec a Čermák z 4. přednášky, populace N=5 pozorování
rm(list=ls())
x<-c(3,5,8,8,12)
y=c(0,0,2,3,3)

# # # Proveďte výběr pro prostý náhodný výběr bez vracení o rozsahu n=3
# # # Určíme N a n.
N=length(y)
n=3
N
n
U=1:N
# # # Pomocný vektor U=c(1,2,...,N) pro provedení výběru
U<-seq(1:N)
# # # Provedeme výběr n=3 z N=5 jednotek
set.seed(123)
s=sample(U,size=n,replace=FALSE)
s
# # # Uložíme si hodnoty proměnných x, y vybraných n=3 jednotek
s_x=x[s]
s_y=y[s]
# # # Výpočet populačních charakteristik
mi_x=mean(x)
mi_y=mean(y)
r_xy=cor(x,y)

plot(x,y)
# # # Odhadněte populační průměr proměnné y pomocí poměrového odhadu r2
# # # Odhad poměru r2
r2=mean(s_y)/mean(s_x)
# # # Odhad populačního průměru mi_hat_r2
mi_hat_r2=r2*mi_x
mi_hat_r2
# # # Populacni prumer
mi_y
# # # Vyberovy prumer
mean(s_y)
# # # Odhadete rozptyl odhadu průměru mi_hat_r2
res_hat_r2=s_y-r2*s_x
res_hat_r2
D_hat_r2=(1-n/N)*var(res_hat_r2)/n
D_hat_r2
# # # Upraveny rozptyl
D_hat_r2_adj=D_hat_r2*(mi_x/mean(s_x))**2
D_hat_r2_adj
# # # Pro 90% spolehlivost spočtěte oboustranný interval spolehlivosti pro populační průměr
# # # Určení kvantilu pro interval spolehlivosti
spol=0.9
alpha=1-(1-spol)/2
st_vol=n-1
z_alpha=qt(p=alpha,df=n-1)
z_alpha

CI_meanY_r2=c(mi_hat_r2-z_alpha*sqrt(D_hat_r2),mi_hat_r2+z_alpha*sqrt(D_hat_r2))
CI_meanY_r2

CI_meanY_r2_adj=c(mi_hat_r2-z_alpha*sqrt(D_hat_r2_adj),mi_hat_r2+z_alpha*sqrt(D_hat_r2_adj))
CI_meanY_r2_adj
# # # Porovnání s intervalem spolehlivosti pro prostý náhodný výběr
CI_meanY=c(mean(s_y)-z_alpha*sd(s_y)*sqrt(1/n-1/N),mean(s_y)+z_alpha*sd(s_y)*sqrt(1/n-1/N))
CI_meanY