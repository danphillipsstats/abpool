# Generate data
set.seed(1)
n <- 400; nobs <- 40
X <- rnorm(n); Z <- sqrt(0.9)*rnorm(n) + sqrt(0.1)*X
beta_0 <- 0; beta_X <- 0.8; beta_Z <- 0.2
eta <- beta_0 + X*beta_X + Z*beta_Z
Y <- rbinom(n, size = 1, p = exp(eta)/(1 + exp(eta)) )
X[1:(n-nobs)] <- NA
log.data <- data.frame(Y = Y, X = X, Z = Z)
# Impute using mice (200 imputations)
m <- 200
imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
fits.log.200 <- with(imp, glm(Y ~ X + Z, family = binomial))
# Fit ABpool
abpool.log.200 <- abpool(fits.log.200, dfcom = Inf)
# Compare ABpool and mice in a QQ plot
qqplot_rubin_abpool(abpool.log.200, parm = "X")
# Observed discrepancy: increase number of imputations and proceed using ABpool
m <- 1000
imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
fits.log.1000 <- with(imp, glm(Y ~ X + Z, family = binomial))
abpool.log.1000 <- abpool(fits.log.1000)
samples <- abpool.log.1000$samples[,"X"]
# Estimate
mean(samples)
# 95% credible interval
confint(abpool.log.1000)["X",]

# ABpool and Rubin's rules equivalent
set.seed(1)
n <- 220; nobs <- 200
X <- rnorm(n); Z <- sqrt(0.9)*rnorm(n) + sqrt(0.1)*X
beta_0 <- 0; beta_X <- 0.2; beta_Z <- 0.2
eta <- beta_0 + X*beta_X + Z*beta_Z
Y <- rnorm(n, mean = eta)
X[1:(n-nobs)] <- NA
lin.data <- data.frame(Y = Y, X = X, Z = Z)
# Impute using mice (200 imputations)
m <- 200
imp <- mice::mice(lin.data, m = m, method = "norm", print = FALSE)
fits.lin.200 <- with(
  imp,
  lm(Y ~ X + Z)
)
# Fit ABpool
abpool.lin.200 <- abpool(fits.lin.200)
# Compare ABpool and mice in a QQ plot
qqplot_rubin_abpool(abpool.lin.200, parm = "X")
# No meaningful discrepancy: proceed using Rubin's rules
pool.lin <- mice::pool(fits.lin.200)
summary(pool.lin, conf.int = TRUE)[2,c("conf.low","conf.high")]


# Longer versions:

# Generate data
set.seed(1)
n <- 400; nobs <- 40
X <- rnorm(n); Z <- sqrt(0.9)*rnorm(n) + sqrt(0.1)*X
beta_0 <- 0; beta_X <- 0.8; beta_Z <- 0.2
eta <- beta_0 + X*beta_X + Z*beta_Z
Y <- rbinom(n, size = 1, p = exp(eta)/(1 + exp(eta)) )
X[1:(n-nobs)] <- NA
log.data <- data.frame(Y = Y, X = X, Z = Z)
# Impute using mice (200 imputations)
m <- 200
imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
fits.log.200 <- with(
  imp,
  glm(Y ~ X + Z, family = binomial)
)
# Fit ABpool
abpool.log.200 <- abpool(fits.log.200, dfcom = Inf)
# Compare ABpool and mice in a QQ plot
qqplot_rubin_abpool(abpool.log.200, parm = "X")
# Observed discrepancy: increase number of imputations and perform inference with ABpool
# Impute using mice
m <- 1000
imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
fits.log.1000 <- with(
  imp,
  glm(Y ~ X + Z, family = binomial)
)
abpool.log.1000 <- abpool(fits.log.1000)
samples <- abpool.log.1000$samples[,"X"]
# Estimate
mean(samples)
# 95% credible interval
confint(abpool.log.1000)["X",]

# Compare with Q--Q plot (1000 imputations)
qqplot_rubin_abpool(abpool.log.1000, parm = "X")
# Compare estimated posterior densities
abpool_density <- density(abpool.log.1000$samples[,"X"])
pool.log <- mice::pool(fits.log.1000)
# Plot density comparison
outcome_cols <- palette("Okabe-Ito")[c(2,3)]
names(outcome_cols) <- c("ABpool","Rubin")
plot(abpool_density, col = outcome_cols[1], lwd = 2, main = "Posterior density")
rubin_mean <- pool.log$pooled$estimate[2]
rubin_scale <- sqrt(pool.log$pooled$t[2])
rubin_df <- pool.log$pooled$df[2]
rubin_density <- dt((abpool_density$x - rubin_mean)/rubin_scale,df = rubin_df)/rubin_scale
lines(rubin_density~abpool_density$x, col = outcome_cols[2], lwd = 2)
legend(x=5, y = 0.4,legend = names(outcome_cols),
       col = outcome_cols,
       lty = 1,
       lwd = 2)
# Compare ABpool uncertainty interval to Rubin's uncertainty interval
confint(abpool.log.1000)["X",]
summary(pool.log, conf.int = TRUE)[2,c("conf.low","conf.high")]

# ABpool and Rubin's rules equivalent
set.seed(1)
n <- 220; nobs <- 200
X <- rnorm(n); Z <- sqrt(0.9)*rnorm(n) + sqrt(0.1)*X
beta_0 <- 0; beta_X <- 0.2; beta_Z <- 0.2
eta <- beta_0 + X*beta_X + Z*beta_Z
Y <- rnorm(n, mean = eta)
X[1:(n-nobs)] <- NA
lin.data <- data.frame(Y = Y, X = X, Z = Z)
# Impute using mice (200 imputations)
m <- 200
imp <- mice::mice(lin.data, m = m, method = "norm", print = FALSE)
fits.lin.200 <- with(
  imp,
  lm(Y ~ X + Z)
)
# Fit ABpool
abpool.lin.200 <- abpool(fits.lin.200)
# Compare ABpool and mice in a QQ plot
qqplot_rubin_abpool(abpool.lin.200, parm = "X")
# No meaningful discrepancy: perform inference using Rubin's rules
pool.lin <- mice::pool(fits.lin.200)
summary(pool.lin, conf.int = TRUE)[2,c("conf.low","conf.high")]

# Impute using mice with greater number of imputations
m <- 1000
imp <- mice::mice(lin.data, m = m, method = "norm", print = FALSE)
fits.lin.1000 <- with(
  imp,
  lm(Y ~ X + Z)
)
abpool.lin.1000 <- abpool(fits.lin.1000)
# Compare with Q--Q plot (1000 imputations)
qqplot_rubin_abpool(abpool.lin.1000, parm = "X")
# Compare estimated posterior densities
abpool_density <- density(abpool.lin.1000$samples[,"X"])
pool.lin <- mice::pool(fits.lin.1000)
# Plot density comparison
outcome_cols <- palette("Okabe-Ito")[c(2,3)]
names(outcome_cols) <- c("ABpool","Rubin")
plot(abpool_density, col = outcome_cols[1], lwd = 2, main = "Posterior density")
rubin_mean <- pool.lin$pooled$estimate[2]
rubin_scale <- sqrt(pool.lin$pooled$t[2])
rubin_df <- pool.lin$pooled$df[2]
rubin_density <- dt((abpool_density$x - rubin_mean)/rubin_scale,df = rubin_df)/rubin_scale
lines(rubin_density~abpool_density$x, col = outcome_cols[2], lwd = 2)
legend(x=5, y = 0.4,legend = names(outcome_cols),
       col = outcome_cols,
       lty = 1,
       lwd = 2)
# Compare ABpool uncertainty interval to Rubin's uncertainty interval
confint(abpool.lin.1000)["X",]
summary(pool.lin, conf.int = TRUE)[2,c("conf.low","conf.high")]
# The small differences observed are likely due primarily to Monte-Carlo error.
