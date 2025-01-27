### MY FUNCTIONS

## PROBABILITY
# n = total number of trials
# r = number of successful trials


# PERMUTATION = order matters
perm = function(n, r) {
  ans = factorial(n) / factorial(n-r)
  print(ans)
}
# with replacement
permReplace = function(n, r) {
  ans = n**r
  print(ans)
}


# COMBINATION = order doesn't matter
comb = function(n, r) {
  ans = factorial(n) / (factorial(n-r)*factorial(r))
  print(ans)
}
# with replacement
combReplace = function(n, r) {
  ans = factorial(n+r-1) / (factorial(n-1)*factorial(r))
  print(ans)
}



## Probability Mass Function
# E[X] = Σxi.P(xi)
expval <- function(pdata){
  ev <- sum(pdata[,1] * pdata[,2])
  print(ev)
}

# Var[X] = Σ(xi-μ)^2.P(xi)
var <- function(pdata){
  ev <- sum(pdata[,1] * pdata[,2])
  var <- sum((pdata[,1] - ev)^2 * pdata[,2])
  print(var)
}


## BERNOULLI TRIALS
# (q = 1-p)
# E[X] = p
# Var[X] = q.p		
BTvar <- function(p){
  BTvar <- (1-p) * p
  print(BTvar)
}


## GEOMETRIC DISTRIBUTION
# P(X) = q^(x-1).p
Gprob <- function(p, x){
  q <- 1 - p
  Gprob <- q^(x-1) * p
  print(Gprob)
}

# E[X] = x.qx-1.p = 1/p
Gexpval <- function(p){
  Gexpval <- 1/p
  print(Gexpval)
}

# Var[X] = q/p^2
Gvar <- function(p){
  q <- 1 - p
  Gvar <- q/p^2
  print(Gvar)
}


## BINOMIAL DISTRIBUTION
# probability = C(r, n) * p^r * (1 - p)^(n - r)
Bprob <- function(p, r, n){
  Bexpval <- choose(n, r) * p^r * (1 - p)^(n - r)
  print(Bexpval)
}
# x = number of successes, n = 100, p = .1
dbinom(0, 100, .1)

# E[X] = p^r.(1-p)^n-r
Bexpval <- function(p, n, r){
  Bexpval <- p^r * (1-p)^(n-r)
  print(Bexpval)
}

# Var[X] = pnq
Bvar <- function(p, n, q){
  Bvar <- p * n * q
  print(Bvar)
}


## POISSON DISTRIBUTION
# P(X=r) f(r) = λ^r - e^-r / r!
Pprob <- function(λ, r){
  Pprob <- exp(-λ) * (λ^r) / factorial(r)
  print(Pprob)
}
Pprob(.25, 8)
# x = 0 number of successes, lambda = 2
dpois(0,2)
dpois(2, 5)
dpois(4, 4.5)



## Z-SCORE
z <- (x-mu)/(sd/sqrt(n))
z <- (28-30.5)/(2.5/sqrt(115))
z
# for proportional tests
prop.z <- (p.hat-p)/sqrt((p*q)/n)
prop.z <- (.29-.25)/sqrt((.25*.75)/1000)
prop.z

## P-VALUE
p.value <- 2 * pnorm(-abs(z))
p.value <- 2 * pnorm(-abs(prop.z))
p.value

## LINEAR MODEL
lm <- lm(y ~ x)
sum.lm <- summary(lm)
slope <- slm$coefficients[2]
intercept <- slm$coefficients[1]
cat("y =", slope, "x", "+", intercept)



## CHI-SQUARE TEST
# Function to calculate expected values
expected.values <- function(observed.data) {
  col.totals <- colSums(observed.data)
  row.totals <- rowSums(observed.data)
  grand.total <- sum(observed.data)
  expected.data <- outer(row.totals, col.totals) / grand.total
  return(as.data.frame(expected.data, row.names = rownames(observed.data),
                       col.names = colnames(observed.data)))
}

# Function to perform a chi-squared test
chi.squared <- function(observed.data, expected.data, alpha = 0.05) {
  result <- chisq.test(observed.data, expected.data)
  print(result)
  # Calculate the chi-squared cut-off value (rejection region)
  DoF <- nrow(observed.data) - 1
  cutoff <- qchisq(1 - alpha, DoF)
  
  # Print the test results
  cat("\n\nTest Statistic (X-squared):", result$statistic, "\n")
  cat("Chi-squared Cutoff:", cutoff, "\n")
  # Determine independence
  if (result$statistic > cutoff) {
    cat("Conclusion: The variables are dependent (reject the null hypothesis).\n")
  } else {
    cat("Conclusion: The variables are independent (fail to reject the null hypothesis).\n")
  }
}

## HERE'S AN EXAMPLE
Observed <- data.frame(satisfied = c(20, 40, 90), not.satisfied = c(80, 60, 10))
Expected <- expected.values(Observed)
chi.squared(Observed, Expected)
