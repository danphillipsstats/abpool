# mice - linear regression
set.seed(1)
require(mice)
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X)
m <- 200
impute.mice <- mice(data.lin, m = m, method = "norm", print=FALSE)
fits.lin.mice <- with(impute.mice, lm(Y~X))
# mice - logistic regression
set.seed(1)
n <- 100
X <- rnorm(n)
Z <- 0.3*X + rnorm(n)
Y <- rbinom(n,size=1,prob=plogis(1+3*X+Z + rnorm(n)))
X[1:50] <- NA
m <- 200
data.log <- data.frame(Y=Y,X=X,Z=Z)
impute.mice <- mice(data.log, m = 200, method = "norm", print=FALSE)
fits.log.mice <- with(impute.mice, glm(Y~X, family = "binomial"))
# mice - Cox regression
set.seed(1)
require(survival)
n <- 500
X <- rnorm(n)
Z <- 0.3*X + rnorm(n)
Y <- rexp(n,rate=exp(-1+3*X+Z+ 0.5*X^2))
# Administratively censor at 100
event <- rep(1,length(Y))
event[which(Y>100)] <- 0
Y[cens==1] <- 100
X[1:50] <- NA
data.cox <- data.frame(Y=Y,X=X,X_square=X^2,Z=Z,event=event)
data.cox$cumhaz <- nelsonaalen(data.cox,timevar=Y,statusvar=event)
pred <- mice::make.predictorMatrix(data.cox)
pred[, "Y"] <- 0 # do not predict using Y
meth <- make.method(data.cox)
meth["X"] <- "norm"
meth["X_square"] <- "~I(X^2)"
impute.cox.mice <- mice(data.cox, m = 200, method = "norm", predictorMatrix = pred, print=FALSE)
fits.cox.mice <- with(impute.cox.mice, survival::coxph(survival::Surv(Y,event)~poly(X,2)+Z))


##################################################
# Input validation
################
# object
test_that("object accepts valid values", {
  # Linear regression
  # mira object
  expect_no_error(
    abpool(fits.lin.mice, dfcom=Inf)
  )
  # list object
  expect_no_error(
    abpool(fits.lin.mice$analyses, dfcom=Inf)
  )
  # Logistic regression
  # mira object
  expect_no_error(
    abpool(fits.log.mice, dfcom=Inf)
  )
  # list object
  expect_no_error(
    abpool(fits.log.mice$analyses, dfcom=Inf)
  )
  # Cox regression
  # mira object
  expect_no_error(
    abpool(fits.cox.mice, dfcom=Inf)
  )
  # list object
  expect_no_error(
    abpool(fits.cox.mice$analyses, dfcom=Inf)
  )
})
