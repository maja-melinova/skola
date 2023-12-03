library(tidyverse)
library(mvtnorm)
library(plotly)
library(MASS)
library(readxl)

n <- 1000

sigma_x1 <- matrix(c(18,-42,-42,260), ncol = 2)
sigma_x1_x2 <- matrix(c(6.57, -2.82, -2.82, 75.00), ncol = 2)

means_x1 <- c(48, 61)
means_x1_x2_1 <- c(60.25, 218.03)
means_x1_x2_2 <- c(26.52, 158.10)


set.seed(42)
x1 <- rmvnorm(n = n, mean = means_x1, sigma = sigma_x1)
d_x1 <- data.frame(x1)
set.seed(42)
x1_x2_1 <- rmvnorm(n = n, mean = means_x1_x2_1, sigma = sigma_x1_x2)
d_x1_x2_1 <- data.frame(x1_x2_1)
set.seed(42)
x1_x2_2 <- rmvnorm(n = n, mean = means_x1_x2_2, sigma = sigma_x1_x2)
d_x1_x2_2 <- data.frame(x1_x2_2)

## 2D grafy
p_x1 <- plot_ly(d_x1, x = ~X1, y = ~X2)
add_histogram2dcontour(p_x1)

p_x1_x2_1 <- plot_ly(d_x1_x2_1, x = ~X1, y = ~X2)
add_histogram2dcontour(p_x1_x2_1)

p_x1_x2_2 <- plot_ly(d_x1_x2_2, x = ~X1, y = ~X2)
add_histogram2dcontour(p_x1_x2_2)

#3D grafy
dens_x1 <- kde2d(d_x1$X1, d_x1$X2)
plot_ly(x = dens_x1$x,
        y = dens_x1$y,
        z = dens_x1$z) %>% add_surface()

dens_x1_x2_1 <- kde2d(d_x1_x2_1$X1, d_x1_x2_1$X2)
plot_ly(x = dens_x1_x2_1$x,
        y = dens_x1_x2_1$y,
        z = dens_x1_x2_1$z) %>% add_surface()

dens_x1_x2_2 <- kde2d(d_x1_x2_2$X1, d_x1_x2_2$X2)
plot_ly(x = dens_x1_x2_2$x,
        y = dens_x1_x2_2$y,
        z = dens_x1_x2_2$z) %>% add_surface()

### Náhodný výběr
data <- read_excel("data.xlsx")
data$seriousness <- as.numeric(data$seriousness)
data2 <- as.data.frame(cbind(data$length, data$feeling, data$age, data$distance, data$seriousness))
colnames(data2) <- c("doba", "pocit", "vek", "vzdalenost", "vaznost")

vektor_prumeru <- colMeans(data2)
vektor_prumeru
vyberova_kov <- cov(data2)
vyberova_kov
wishartova_matice <- vyberova_kov * (length(data)-1)

x1_vyber_prumery <- round(vektor_prumeru[c(1, 2)], 2)
x2_vyber_prumery <- round(vektor_prumeru[-c(1, 2)], 2)

x1_vyber_kov <- round(vyberova_kov[1:2, 1:2], 2)
x2_vyber_kov <- round(vyberova_kov[3:5, 3:5], 2)

x1_porovnani_prum <- paste(x1_vyber_prumery, c("(48)", "(61)"))
names(x1_porovnani_prum) <- c("doba", "pocit")
x1_porovnani_prum

x2_porovnani_prum <- paste(x2_vyber_prumery, c("(35)", "(45)", "(2.5)"))
names(x2_porovnani_prum) <- c("vek", "vzdalenost", "vaznost")
x2_porovnani_prum


x1_porovnani_kov <- as.data.frame(matrix(paste(x1_vyber_kov, c("(18)", "(-42)", "(-42)", "(260)")), nrow = 2, ncol = 2))
rownames(x1_porovnani_kov) <- c("doba", "pocit")
colnames(x1_porovnani_kov) <- c("doba", "pocit")
x1_porovnani_kov

x2_porovnani_kov <- as.data.frame(matrix(paste(x2_vyber_kov, c("(70)", "(110)", "(1.3)", "(110)", "(2260)", "(4.5)", "(1.3)", "(4.5)", "(0.1)")), nrow = 3, ncol = 3))
colnames(x2_porovnani_kov) <- c("vek", "vzdalenost", "vaznost")
rownames(x2_porovnani_kov) <- c("vek", "vzdalenost", "vaznost")
x2_porovnani_kov







