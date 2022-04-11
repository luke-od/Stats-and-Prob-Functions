
z.test <- function(x, mu, s, n, tail){
  z <- (x-mu)/(s/sqrt(n))
  if(tail==1) return(cat("z-score =", z, "\np-value", pnorm(-abs(z))))
  if(tail==2) return(cat("z-score =", z, "\np-value", 2*pnorm(-abs(z))))
  if(tail != 1 | tail != 2) return("can only be 1 or 2 tailed")
}

z.test(6, 5, 2, 20, 2)


z.tested <-function(z, tail){
  if(tail==1) return(cat("z-score =", z, "\np-value =", pnorm(-abs(z))))
  if(tail==2) return(cat("z-score =", z, "\np-value =", 2*pnorm(-abs(z))))
  if(tail !=1 | tail !=2) return("can only be 1 or 2 tailed")
}

z.tested(1.96, 1)
