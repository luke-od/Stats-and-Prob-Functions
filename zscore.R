# x = a specific value u want to test
# mu = population mean
# s = standard deviation
# n = sample size
# tail = 1 or 2 tailed test

z.test <- function(x, mu, s, n, tail){
  z <- (x-mu)/(s/sqrt(n))
  if(tail==1) return(cat("z-score =", z, "\np-value", pnorm(-abs(z))))
  if(tail==2) return(cat("z-score =", z, "\np-value", 2*pnorm(-abs(z))))
  if(tail != 1 | tail != 2) return("can only be 1 or 2 tailed")
}

z.test(6, 5, 2, 20, 2)


p.value <-function(z, tail){
  if(tail==1) return(cat("z-score =", z, "\np-value =", pnorm(-abs(z))))
  if(tail==2) return(cat("z-score =", z, "\np-value =", 2*pnorm(-abs(z))))
  if(tail !=1 | tail !=2) return("can only be 1 or 2 tailed")
}

p.value(1.96, 1)
# feel free to stop it from printing out the z-score u just put in
#\n just puts it on a new line
