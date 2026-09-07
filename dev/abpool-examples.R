# list - linear regression
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
m <- 200
fits <- lapply(1:m, function(i){
  X[1:50] <- rnorm(50)
  lm(Y~X)
})

lapply(fits,coef)
lapply(fits,vcov)
lapply(fits,df.residual)

# list - logistic regression
n <- 100
X <- rnorm(n)
Y <- rbinom(n,size=1,prob=plogis(1+3*X + rnorm(n)))
X[1:50] <- NA
m <- 200
fits <- lapply(1:m, function(i){
  X[1:50] <- rnorm(50)
  glm(Y~X, family="binomial")
})

lapply(fits,coef)
lapply(fits,vcov)
lapply(fits,df.residual)

# list - Cox regression

# mice
library(mice)
imp <- mice(nhanes, m = 200, seed = 123, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
is.mira(fit)
is.list(fit) # TRUE
fit$analyses
est1 <- pool(fit)
?mice::pool

