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
# mice - logistic regression with bsplines
set.seed(1)
n <- 1000
X <- rnorm(n)
Z <- 0.3*X + rnorm(n)
Y <- rbinom(n,size=1,prob=plogis(1+3*X+Z + rnorm(n)))
X[1:50] <- NA
m <- 200
data.log <- data.frame(Y=Y,X=X,Z=Z)
impute.mice <- mice(data.log, m = 200, method = "norm", print=FALSE)
fits.log.mice <- with(impute.mice, glm(Y~splines::bs(X, knots = c(-0.5,0.5), degree = 3), family = "binomial"))
# mice - Cox regression with square
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
# List of estimates and variances
list_estimates <- list(c(x = 1, z = 2), c(x = 3, z = 4), c(x = 5, z = 6) )
variance_mat <- matrix(c(1,0.1,0.1,2),2,2)
list_variances <- list(variance_mat, variance_mat, variance_mat)

##################################################
# Input validation
################
# object
test_that("object accepts valid models - type mira and list", {
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
test_that("object rejects invalid values", {
  # 0 imputations
  expect_error(
    abpool(fits.lin.mice$analyses[0], dfcom=Inf)
  )
  # Not a list
  expect_error(
    abpool(fits.lin.mice$analyses[[1]], dfcom=Inf)
  )
  # List of wrong structure
  expect_error(
    abpool(list(fits.lin.mice$analyses[1],fits.lin.mice$analyses[2]), dfcom=Inf)
  )
  # List not of models
  expect_error(
    abpool(list(1:3), dfcom=Inf)
  )
  # List of estimates and variances
  expect_error(
    abpool(list_estimates,list_variances, dfcom=Inf, J = 1)
  )
  # List of estimates and variances
  expect_error(
    abpool(list(list_estimates,list_variances), dfcom=Inf, J = 1)
  )
})
# parameters
test_that("parameters accepts valid inputs - character", {
  # NULL
  expect_no_error(
    abpool(fits.lin.mice, parameters=NULL, dfcom=Inf)
  )
  # all
  expect_no_error(
    abpool(fits.lin.mice, parameters=c("(Intercept)","X"), dfcom=Inf)
  )
  # one parameter
  expect_no_error(
    abpool(fits.lin.mice, parameters=c("X"), dfcom=Inf)
  )
  # spline
  expect_no_error(
    abpool(fits.log.mice, parameters=c("splines::bs(X, knots = c(-0.5, 0.5), degree = 3)1"), dfcom=Inf)
  )
  # poly
  expect_no_error(
    abpool(fits.cox.mice, parameters=c("poly(X, 2)2"), dfcom=Inf)
  )
})
# parameters
test_that("parameters accepts valid inputs - numeric", {
  # all
  expect_no_error(
    abpool(fits.lin.mice, parameters=c(1,2), dfcom=Inf)
  )
  # one parameter
  expect_no_error(
    abpool(fits.lin.mice, parameters=c(2), dfcom=Inf)
  )
  # spline
  expect_no_error(
    abpool(fits.log.mice, parameters=c(1:3), dfcom=Inf)
  )
  # poly
  expect_no_error(
    abpool(fits.cox.mice, parameters=c(2:3), dfcom=Inf)
  )
})

# parameters
test_that("parameters rejects valid inputs", {
  expect_error(
    abpool(fits.lin.mice, parameters="all", dfcom=Inf)
  )
  # extras
  expect_error(
    abpool(fits.lin.mice, parameters=c("(Intercept)","X","Z","X^2"), dfcom=Inf)
  )
  # one incorrect parameter
  expect_error(
    abpool(fits.lin.mice, parameters=c("R"), dfcom=Inf)
  )
  # 0
  expect_error(
    abpool(fits.lin.mice, parameters=c(0), dfcom=Inf)
  )
  # negative
  expect_error(
    abpool(fits.lin.mice, parameters=-3, dfcom=Inf)
  )
  # inf
  expect_error(
    abpool(fits.lin.mice, parameters=Inf, dfcom=Inf)
  )
  # inf
  expect_error(
    abpool(fits.lin.mice, parameters=NA, dfcom=Inf)
  )
  # spline
  expect_error(
    abpool(fits.log.mice, parameters=1:20, dfcom=Inf)
  )
  # duplicates
  expect_error(
    abpool(fits.lin.mice, parameters=rep(1,3), dfcom=Inf)
  )
  # duplicates
  expect_error(
    abpool(fits.lin.mice, parameters=rep("X",3), dfcom=Inf)
  )
  # booleian
  expect_error(
    abpool(fits.lin.mice, parameters=TRUE, dfcom=Inf)
  )
  # booleian
  expect_error(
    abpool(fits.lin.mice, parameters=FALSE, dfcom=Inf)
  )
  # list
  expect_error(
    abpool(fits.lin.mice, parameters=list("(Intercept)","X"), dfcom=Inf)
  )
  # matrix
  expect_error(
    abpool(fits.lin.mice, parameters=matrix(c(1,2),nrow=1,ncol=2), dfcom=Inf)
  )
})

