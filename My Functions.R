### MY FUNCTIONS
# n = total number of trials
# r = number of successful trials

num <- c(7, 3, 10, 9, 8, 2, 0)
mean(num)

choose(56,8)

dbinom(2, 18, .150)


# permutation = order matters
perm = function(n, r) {
  ans = factorial(n) / factorial(n-r)
  print(ans)
}

permReplace = function(n, r) {
  ans = n**r
  print(ans)
}


# combination = order doesn't matter
comb = function(n, r) {
  ans = factorial(n) / (factorial(n-r)*factorial(r))
  print(ans)
}

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


## CHI SQUARE TEST

