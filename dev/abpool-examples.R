# list - linear regression
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
m <- 200
fits.lin <- lapply(1:m, function(i){
  X[1:50] <- rnorm(50)
  lm(Y~X)
})
coefnames <- lapply(fits.lin,function(x) names(coef(x)))
colnames <- lapply(fits.lin,function(x) colnames(vcov(x)))
rownames <- lapply(fits.lin,function(x) rownames(vcov(x)))
all(sapply(1:m, function(i) all(coefnames[[i]]==colnames[[i]]) && all(coefnames[[i]]==rownames[[i]])))

dimnames[[1]]
unlist(dimnames)
lapply(fits.lin,df.residual)

# list - logistic regression
require(mice)
require(survival)
n <- 100
X <- rnorm(n)
Y <- rbinom(n,size=1,prob=plogis(1+3*X + rnorm(n)))
X[1:50] <- NA
m <- 200
fits.log <- lapply(1:m, function(i){
  X[1:50] <- rnorm(50)
  glm(Y~X, family="binomial")
})

lapply(fits.log,coef)
lapply(fits.log,vcov)
lapply(fits.log,df.residual)

# list - Cox regression
Hmisc::getHdata(support) # getHdata(support, "all") to view data dictionary
cox.support.form.char <- "Surv(d.time, death) ~ age + num.co + scoma + alb + bili + pafi + crea + meanbp + hrt + resp + temp + adlsc + dzgroup"
support.data <- support[,all.vars(as.formula(cox.support.form.char))]
m <- 200
t1 <- Sys.time()
support.data$H0_NA <- mice::nelsonaalen(support.data, timevar = "d.time", statusvar = "death")
pred <- mice::make.predictorMatrix(support.data)
# choose methods: do not impute death or H_NA; impute others with pmm
method <- make.method(support.data)
for (v in names(method)) {
  method[v] <- "pmm"
}
pred[, "d.time"] <- 0 # do not predict using d.time
# use H0_NA and death to predict other vars
pred[, "H0_NA"] <- 1
pred[, "death"] <- 1
pred["H0_NA", ] <- 0
pred["death", ] <- 0
imp_data_support <- mice(support.data, m = m, method = method, predictorMatrix = pred)
support.cox.imp <- with(imp_data_support,coxph(as.formula(cox.support.form.char)))
mice::pool
support.cox.imp
lapply(support.cox.imp$analyses,coef)
lapply(support.cox.imp$analyses,vcov)
lapply(support.cox.imp$analyses,df.residual)

# mice
library(mice)
imp <- mice(nhanes, m = 200, seed = 123, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
is.mira(fit)
is.list(fit) # TRUE
fit$analyses
est1 <- pool(fit)
?mice::pool

