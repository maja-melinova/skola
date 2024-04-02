
vystup <- as.data.frame(matrix(nrow = 0, ncol = 1))

for(r in 1:200){
  for(m in 1:12){
    for(d in 1:31){
      R <- r
      M <- m
      D <- d
      
      hodnoty <- c(R - 50, 2*M - 3, 15, 3*M - 6, 22, D + 6, -12, D-8, R/2)
      vystup <- rbind(vystup, toString(paste(hodnoty, sep=" ", collapse = " ")))
      
    }
  }
}

vystup[sample(1:74400, 50),]




