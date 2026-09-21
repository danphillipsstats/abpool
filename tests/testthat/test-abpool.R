#####
# Models that work
# mice - linear regression
set.seed(1)
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X)
m <- 200
impute.mice <- mice::mice(data.lin, m = m, method = "norm", print=FALSE)
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
impute.mice <- mice::mice(data.log, m = 200, method = "norm", print=FALSE)
fits.log.mice <- with(impute.mice, glm(Y~splines::bs(X, knots = c(-0.5,0.5), degree = 3), family = "binomial"))
# mice - Cox regression with square
set.seed(1)
n <- 500
X <- rnorm(n)
Z <- 0.3*X + rnorm(n)
Y <- rexp(n,rate=exp(-1+3*X+Z+ 0.5*X^2))
event <- rep(1,length(Y))
event[which(Y>100)] <- 0 # Administratively censor at 100
Y[event==0] <- 100
X[1:50] <- NA
data.cox <- data.frame(Y=Y,X=X,X_square=X^2,Z=Z,event=event)
data.cox$cumhaz <- mice::nelsonaalen(data.cox,timevar=Y,statusvar=event)
pred <- mice::make.predictorMatrix(data.cox)
pred[, "Y"] <- 0 # do not predict using Y
meth <- make.method(data.cox)
meth["X"] <- "norm"
meth["X_square"] <- "~I(X^2)"
impute.cox.mice <- mice::mice(data.cox, m = 200, method = "norm", predictorMatrix = pred, print=FALSE)
fits.cox.mice <- with(impute.cox.mice, survival::coxph(survival::Surv(Y,event)~poly(X,2)+Z))
# List of estimates and variances
list_estimates <- list(c(x = 1, z = 2), c(x = 3, z = 4), c(x = 5, z = 6) )
variance_mat <- matrix(c(1,0.1,0.1,2),2,2)
list_variances <- list(variance_mat, variance_mat, variance_mat)
# mice - linear regression, m = 1
set.seed(1)
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X)
impute.mice <- mice::mice(data.lin, m = 1, method = "norm", print=FALSE)
fits.lin.mice.one <- with(impute.mice, lm(Y~X))
#####
# models that break
# mice + lmer - coef doesn't work properly for lmer
set.seed(1)
n_id <- 100
n_rep <- 3
id <- factor(rep(seq_len(n_id), each = n_rep))
X <- rnorm(n_id)
u <- rnorm(n_id)
Y <- 1 + 2 * rep(X, each = n_rep) + rep(u, each = n_rep) +
  rnorm(n_id * n_rep)
X[1:30] <- NA
data.lmer <- data.frame(id = id, X = rep(X, each = n_rep), Y = Y)
pred <- mice::make.predictorMatrix(data.lmer)
pred[, "id"] <- 0
impute.mice <- mice::mice(data.lmer, m = 200, predictorMatrix = pred, print = FALSE)
fits.lmer.mice <- with(
  impute.mice,
  lme4::lmer(Y ~ X + (1 | id))
)
# mice - linear regression, estimates fail
set.seed(1)
require(mice)
n <- 100
X <- rnorm(n)
Z <- rep(1,n)
Y <- 1+3*X + Z + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X)
m <- 200
impute.mice <- mice::mice(data.lin, m = m, method = "norm", print=FALSE)
fits.lin.mice.est.error <- with(impute.mice, lm(Y~X+Z))
# mice - linear regression, df error
set.seed(1)
require(mice)
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X)
m <- 200
impute.mice <- mice::mice(data.lin, m = m, method = "norm", print=FALSE)
impute.mice$imp$X[1,1] <- NA # Make an imputation NA so complete case will ignore this individual in lm, making df.residual = n - p smaller by 1.
fits.lin.mice.df.error <- with(impute.mice, lm(Y~X))
# list - linear regression
n <- 100
X <- rnorm(n)
Z <- rnorm(n)
Y <- 1+3*X + Z + rnorm(n)
X[1:50] <- NA
m <- 200
fits.lin.order.error <- lapply(1:m, function(i){
  X[1:50] <- rnorm(50)
  lm(Y~X + Z)
})
fits.lin.order.error[[m]] <- lm(Y~Z + X) # Different order

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
  # reordered
  expect_no_error(
    abpool(fits.lin.mice, parameters=c("X","(Intercept)"), dfcom=Inf)
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
  # reordered
  expect_no_error(
    abpool(fits.lin.mice, parameters=c(2,1), dfcom=Inf)
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
test_that("parameters rejects invalid inputs", {
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
# df
test_that("df accepts valid inputs", {
  # df infinite
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
  # df finite
  # Linear regression
  # mira object
  expect_no_error(
    abpool(fits.lin.mice, dfcom=6)
  )
  # list object
  expect_no_error(
    abpool(fits.lin.mice$analyses, dfcom=6)
  )
  # Logistic regression
  # mira object
  expect_no_error(
    abpool(fits.log.mice, dfcom=6)
  )
  # list object
  expect_no_error(
    abpool(fits.log.mice$analyses, dfcom=6)
  )
  # Cox regression
  # mira object
  expect_no_error(
    abpool(fits.cox.mice, dfcom=6)
  )
  # list object
  expect_no_error(
    abpool(fits.cox.mice$analyses, dfcom=6)
  )
})
test_that("df rejects invalid inputs", {
  expect_error(
    abpool(fits.lin.mice, dfcom=0)
  )
  expect_error(
    abpool(fits.lin.mice, dfcom=-1)
  )
  expect_error(
    abpool(fits.lin.mice, dfcom=-Inf)
  )
  expect_error(
    abpool(fits.lin.mice, dfcom=NA)
  )
  expect_error(
    abpool(fits.lin.mice, dfcom=NaN)
  )
  # vector
  expect_error(
    abpool(fits.lin.mice$analyses, dfcom=rep(6,length(fits.lin.mice$analyses)))
  )
  # list
  expect_error(
    abpool(fits.lin.mice$analyses, dfcom=as.list(rep(6,length(fits.lin.mice$analyses))))
  )
})
test_that("df extracts correctly where available", {
  # Linear regression
  # mira object
  expect_no_error(
    abpool(fits.lin.mice, dfcom=NULL)
  )
  # list object
  expect_no_error(
    abpool(fits.lin.mice$analyses, dfcom=NULL)
  )
  # Logistic regression
  # mira object
  expect_no_error(
    abpool(fits.log.mice, dfcom=NULL)
  )
  # list object
  expect_no_error(
    abpool(fits.log.mice$analyses, dfcom=NULL)
  )
})
test_that("Error where df can't be extracted", {
  # Cox regression
  # mira object
  expect_error(
    abpool(fits.cox.mice, dfcom=NULL)
  )
  # list object
  expect_error(
    abpool(fits.cox.mice$analyses, dfcom=NULL)
  )
})
# J
test_that("J accepts valid inputs", {
  # J > 1, vector
  # Linear regression
  # mira object
  expect_no_error(
    abpool(fits.lin.mice, dfcom=Inf, J = 2)
  )
  # list object
  expect_no_error(
    abpool(fits.lin.mice$analyses, dfcom=Inf, J = 2)
  )
  # Logistic regression
  # mira object
  expect_no_error(
    abpool(fits.log.mice, dfcom=Inf, J = 2)
  )
  # list object
  expect_no_error(
    abpool(fits.log.mice$analyses, dfcom=Inf, J = 2)
  )
  # Cox regression
  # mira object
  expect_no_error(
    abpool(fits.cox.mice, dfcom=Inf, J = 2)
  )
  # list object
  expect_no_error(
    abpool(fits.cox.mice$analyses, dfcom=Inf, J = 2)
  )
  # J > 1, scalar
  # Linear regression
  # mira object
  expect_no_error(
    abpool(fits.lin.mice, dfcom=Inf, parameters = 1, J = 2)
  )
  # list object
  expect_no_error(
    abpool(fits.lin.mice$analyses, dfcom=Inf, parameters = 1, J = 2)
  )
  # Logistic regression
  # mira object
  expect_no_error(
    abpool(fits.log.mice, dfcom=Inf, parameters = 1, J = 2)
  )
  # list object
  expect_no_error(
    abpool(fits.log.mice$analyses, dfcom=Inf, parameters = 1, J = 2)
  )
  # Cox regression
  # mira object
  expect_no_error(
    abpool(fits.cox.mice, dfcom=Inf, parameters = 1, J = 2)
  )
  # list object
  expect_no_error(
    abpool(fits.cox.mice$analyses, dfcom=Inf, parameters = 1, J = 2)
  )
})
test_that("J rejects invalid inputs", {
  # J = 0
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = 0)
  )
  # J = 0
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = "1")
  )
  # J = NA
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = NA)
  )
  # J = Inf
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = Inf)
  )
  # J = NaN
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = NaN)
  )
  # J = -1
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = -1)
  )
  # J = 1.5
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = 1.5)
  )
  # J = NULL
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = NULL)
  )
  # J = vec
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = rep(1,length(fits.lin.mice$analyses)))
  )
  # J = list
  expect_error(
    abpool(fits.lin.mice, dfcom=Inf, J = as.list(rep(1,length(fits.lin.mice$analyses))))
  )
})
# m = 1
test_that("object accepts valid models - type mira and list", {
  # Linear regression
  # mira object
  expect_no_error(
    abpool(fits.lin.mice.one, dfcom=Inf)
  )
  # list object
  expect_no_error(
    abpool(fits.lin.mice.one$analyses, dfcom=Inf)
  )
})
#########
# models
test_that("model for which coef/vcov fails", {
  # lmer doesn't work for coef
  expect_error(
    abpool(fits.lmer.mice, dfcom=Inf)
  )
})
test_that("model with undefined estimates", {
  expect_error(
    abpool(fits.lin.mice.est.error, dfcom=Inf),
    "estimates.*finite values"
  )
})
test_that("model with different df for different imputations", {
  expect_error(
    abpool(fits.lin.mice.df.error),
    "dfcom.*vary between imputations"
  )
})
test_that("model with different order of parameters", {
  expect_error(
    abpool(fits.lin.order.error),
    "names.*inconsistent across imputations"
  )
})
################################################################################
# Test outputs
#####
# list and mice version coincide
test_that("results from mira and list coincide", {
  # Linear regression
  # Scalar
  # dfcom = Inf, J = 1
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, parameters = "X", dfcom=Inf)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, parameters = "X", dfcom=Inf)
  expect_equal(lm.mira,lm.list)
  # dfcom = Inf, J = 2
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, parameters = "X", dfcom=Inf, J =2)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, parameters = "X", dfcom=Inf, J =2)
  expect_equal(lm.mira,lm.list)
  # dfcom = Inf, J = 1
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, parameters = "X", dfcom=6)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, parameters = "X", dfcom=6)
  expect_equal(lm.mira,lm.list)
  # dfcom = 6, J = 2
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, parameters = "X", dfcom=6, J =2)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, parameters = "X", dfcom=6, J =2)
  expect_equal(lm.mira,lm.list)
  # multivariate
  # dfcom = Inf, J = 1
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, dfcom=Inf)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, dfcom=Inf)
  expect_equal(lm.mira,lm.list)
  # dfcom = Inf, J = 2
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, dfcom=Inf, J =2)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, dfcom=Inf, J =2)
  expect_equal(lm.mira,lm.list)
  # dfcom = Inf, J = 1
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, dfcom=6)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, dfcom=6)
  expect_equal(lm.mira,lm.list)
  # dfcom = 6, J = 2
  set.seed(1)
  lm.mira <- abpool(fits.lin.mice, dfcom=6, J =2)
  set.seed(1)
  lm.list <- abpool(fits.lin.mice$analyses, dfcom=6, J =2)
  expect_equal(lm.mira,lm.list)
})
#####
# Validate outputs (length/dimension, type, names)
lm.J1.scalar <- abpool(fits.lin.mice, dfcom=Inf, parameters = "X", J=1)
lm.J2.scalar <- abpool(fits.lin.mice, dfcom=Inf, parameters = "X", J=2)
lm.J1.multi <- abpool(fits.lin.mice, dfcom=Inf, J=1)
lm.J2.multi <- abpool(fits.lin.mice, dfcom=Inf, J=2)
# Samples
test_that("samples have correct dimensions, type, names", {
  # Length/dimensions
  expect_length(lm.J1.scalar$samples,lm.J1.scalar$m*lm.J1.scalar$J) # Scalar, J = 1
  expect_length(lm.J2.scalar$samples,lm.J2.scalar$m*lm.J2.scalar$J) # Scalar, J > 1
  expect_equal(dim(lm.J1.multi$samples)[1],lm.J1.multi$m*lm.J1.multi$J) # Multi, J = 1
  expect_equal(dim(lm.J2.multi$samples)[1],lm.J2.multi$m*lm.J2.multi$J) # Multi, J > 1
  expect_equal(dim(lm.J1.multi$samples)[2],length(lm.J1.multi$parameters)) # Multi, J = 1
  expect_equal(dim(lm.J2.multi$samples)[2],length(lm.J2.multi$parameters)) # Multi, J > 1
  # type
  expect_true(is.numeric(lm.J1.scalar$samples)) # Scalar, J = 1
  expect_true(is.numeric(lm.J2.scalar$samples)) # Scalar, J > 1
  expect_true(is.numeric(lm.J1.multi$samples)) # Multi, J = 1
  expect_true(is.numeric(lm.J2.multi$samples)) # Multi, J > 1
  expect_true(is.matrix(lm.J1.multi$samples)) # Multi, J = 1
  expect_true(is.matrix(lm.J2.multi$samples)) # Multi, J > 1
  # names
  expect_equal(colnames(lm.J1.multi$samples),lm.J1.multi$parameters) # Multi, J = 1
  expect_equal(colnames(lm.J2.multi$samples),lm.J2.multi$parameters) # Multi, J > 1
})
test_that("estimates have correct dimensions, type, names", {
  # Length/dimensions
  expect_length(lm.J1.scalar$estimates,lm.J1.scalar$m) # Scalar
  expect_equal(length(lm.J1.multi$estimates),lm.J1.multi$m) # Multi
  expect_equal(length(lm.J1.multi$estimates[[1]]),length(lm.J1.multi$parameters)) # Multi
  # type
  expect_true(is.numeric(lm.J1.scalar$estimates)) # Scalar
  expect_true(is.numeric(lm.J1.multi$estimates[[1]])) # Multi
  expect_true(is.list(lm.J1.multi$estimates)) # Multi
  # names
  expect_equal(names(lm.J1.multi$estimates[[1]]),lm.J1.multi$parameters) # Multi
})
test_that("variances have correct dimensions, type, names", {
  # Length/dimensions
  expect_length(lm.J1.scalar$variances,lm.J1.scalar$m) # Scalar
  expect_equal(length(lm.J1.multi$variances),lm.J1.multi$m) # Multi
  expect_equal(dim(lm.J1.multi$variances[[1]])[1],length(lm.J1.multi$parameters)) # Multi
  expect_equal(dim(lm.J1.multi$variances[[1]])[2],length(lm.J1.multi$parameters)) # Multi
  # type
  expect_true(is.numeric(lm.J1.scalar$variances)) # Scalar
  expect_true(is.numeric(lm.J1.multi$variances[[1]])) # Multi
  expect_true(is.matrix(lm.J1.multi$variances[[1]])) # Multi
  expect_true(is.list(lm.J1.multi$variances)) # Multi
  # names
  expect_equal(colnames(lm.J1.multi$variances[[1]]),lm.J1.multi$parameters) # Multi
  expect_equal(rownames(lm.J1.multi$variances[[1]]),lm.J1.multi$parameters) # Multi
})
test_that("m output is correct", {
  expect_equal(abpool(fits.lin.mice)$m,length(fits.lin.mice$analyses))
  expect_equal(abpool(fits.lin.mice$analyses)$m,length(fits.lin.mice$analyses))
})
test_that("J matches input", {
  expect_equal(abpool(fits.lin.mice)$J, 1) # Unspecified gives J = 1
  expect_equal(abpool(fits.lin.mice, J = 2)$J, 2)
})
test_that("dfcom matches input", {
  expect_equal(abpool(fits.lin.mice)$dfcom, df.residual(fits.lin.mice$analyses[[1]])) # Unspecified calculated by df.residual
  expect_equal(abpool(fits.log.mice)$dfcom, df.residual(fits.log.mice$analyses[[1]])) # Unspecified calculated by df.residual
  expect_equal(abpool(fits.lin.mice, dfcom = 2)$dfcom, 2)
  expect_equal(abpool(fits.lin.mice, dfcom = Inf)$dfcom, Inf)
})
test_that("Validate parameters output", {
  expect_equal(abpool(fits.lin.mice)$parameters, names(coef(fits.lin.mice$analyses[[1]]))) # Unspecified
  expect_equal(abpool(fits.lin.mice, parameters = 2)$parameters, names(coef(fits.lin.mice$analyses[[1]]))[2]) # numeric
  expect_equal(abpool(fits.lin.mice, parameters = c(2,1))$parameters, names(coef(fits.lin.mice$analyses[[1]]))[c(2,1)]) # reordered
  expect_equal(abpool(fits.lin.mice, parameters = "X")$parameters, "X") # character
})
test_that("J matches input", {
  expect_equal(lm.J1.scalar$imputation, 1:lm.J1.scalar$m) # J = 1
  expect_equal(lm.J2.scalar$imputation, rep(1:lm.J2.scalar$m,each=lm.J2.scalar$J))
  expect_equal(lm.J1.multi$imputation, 1:lm.J1.multi$m) # J = 1
  expect_equal(lm.J2.multi$imputation, rep(1:lm.J2.multi$m,each=lm.J2.multi$J))
})
#####
# Checking parameters reorder correctly
test_that("Check parameters reorder correctly", {
  expect_equal(abpool(fits.lin.mice, parameters = c(2,1))$estimates[[1]], coef(fits.lin.mice$analyses[[1]])[c(2,1)]) # numeric
  expect_equal(abpool(fits.lin.mice, parameters = c(2,1))$variances[[1]], vcov(fits.lin.mice$analyses[[1]])[c(2,1),c(2,1)]) # numeric
})
