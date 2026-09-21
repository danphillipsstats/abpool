require(mice)
# Main example
# Generate data
set.seed(1)
n <- 400; nobs <- 40
X <- rnorm(n); Z <- sqrt(0.9)*rnorm(n) + sqrt(0.1)*X
beta_0 <- 0; beta_X <- 0.8; beta_Z <- 0.2
eta <- beta_0 + X*beta_X + Z*beta_Z
Y <- rbinom(n, size = 1, p = exp(eta)/(1 + exp(eta)) )
X[1:(n-nobs)] <- NA
log.data <- data.frame(Y = Y, X = X, Z = Z)
m <- 200
# Impute using mice (200 imputations)
imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
fits <- with(
  imp,
  glm(Y ~ X + Z, family = binomial)
)
# Fit ABpool
abpool.out.200 <- abpool(fits)
# Compare ABpool and mice in a QQ plot
qqplot_rubin_abpool(abpool.out.200, parm = "X")
# Observed discrepancy: increase number of imputations and fit with ABpool
# Impute using mice ()
m <- 10000
imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
fits <- with(
  imp,
  glm(Y ~ X + Z, family = binomial)
)
pool.out <- mice::pool(fits)
abpool.out.1000 <- abpool(fits)
qqplot_rubin_abpool(abpool.out.1000, parm = "X")
abpool_density <- density(abpool.out.1000$samples[,"X"])
plot(abpool_density)
lines(dt((abpool_density$x-pool.out$pooled$estimate[2])/sqrt(pool.out$pooled$t[2]),df=pool.out$pooled$df[2])~abpool_density$x)
# mice - linear regression
n <- 100
X <- rnorm(n)
Y <- 1+3*X + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X)
m <- 200
impute.mice <- mice(data.lin, m = 200, method = "norm")
fits.mice <- with(impute.mice, lm(Y~X))
fits.lin <- fits.mice$analyses
coefnames <- lapply(fits.lin,function(x) names(coef(x)))
colnames <- lapply(fits.lin,function(x) colnames(vcov(x)))
rownames <- lapply(fits.lin,function(x) rownames(vcov(x)))

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

# mice + lmer
set.seed(1)
require(lme4)
n_id <- 100
n_rep <- 3
id <- factor(rep(seq_len(n_id), each = n_rep))
X <- rnorm(n_id)
u <- rnorm(n_id)
Y <- 1 + 2 * rep(X, each = n_rep) + rep(u, each = n_rep) +
  rnorm(n_id * n_rep)
X[1:30] <- NA
data.lmer <- data.frame(id = id, X = rep(X, each = n_rep), Y = Y)
impute.mice <- mice(data.lmer, m = 200, method = "norm", print = FALSE)
fits.lmer.mice <- with(
  impute.mice,
  lmer(Y ~ X + (1 | id))
)
abpool(fits.lmer.mice)
# Gives an error, so lmer not currently supported.
