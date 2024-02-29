library(haven)

data3_1 = read_sav("data/data3_1.sav")

summary(data3_1[,-c(1:5)])
qqnorm(data3_1$HRUDNIK, pch = 1, frame = FALSE)
qqline(data3_1$HRUDNIK, col = "steelblue", lwd = 2)