#####
# mice - linear regression
set.seed(1)
require(mice)
n <- 100
X <- rnorm(n); Z <- rnorm(n)
Y <- 1+3*X + Z + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X,Z=Z)
m <- 200
impute.mice <- mice(data.lin, m = m, method = "norm", print=FALSE)
fits.lin.mice <- with(impute.mice, lm(Y~X+Z))
abpool.out.multi <- abpool(fits.lin.mice)
abpool.out.scalar <- abpool(fits.lin.mice, parameters = "X")
###############################################################################
# Testing
######
# Inputs
# object
test_that("object accepts valid inputs", {
  # scalar
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar)
  )
  # multi
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi)
  )
})
# parm
test_that("parm accepts valid inputs", {
  # scalar
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, parm = c(1))
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, parm = c("X"))
  )
  # multi
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = c(1))
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = c(3,2))
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = c(-1))
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = "X")
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = c("Z","X"))
  )
})
test_that("parm rejects invalid inputs", {
  # scalar
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, parm = 2)
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, parm = -1)
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, parm = NA)
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, parm = c("Z")) # "Z" was not in parameters input for abpool, although was a parameter in the model
  )
  # multi
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = -c(1:3))
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = c(-1,3))
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = c("X","click"))
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, parm = NA)
  )
})
test_that("plot.it accepts valid inputs", {
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, plot.it = TRUE)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, plot.it = FALSE)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, plot.it = TRUE)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, plot.it = FALSE)
  )
})
test_that("plot.it rejects invalid inputs", {
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, plot.it = "no")
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, plot.it = 0)
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, plot.it = "no")
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, plot.it = 1)
  )
})
test_that("add_qqline accepts valid inputs", {
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, add_qqline = TRUE)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, add_qqline = FALSE)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, add_qqline = TRUE)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, add_qqline = FALSE)
  )
})
test_that("add_qqline rejects invalid inputs", {
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, add_qqline = "no")
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.scalar, add_qqline = 0)
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, add_qqline = "no")
  )
  expect_error(
    qqplot_rubin_abpool(abpool.out.multi, add_qqline = 1)
  )
})
#####
# Other graphical inputs
test_that("no error when other graphical inputs supplied", {
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, main = "")
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, xlab = "Rubin")
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.scalar, pch=2)
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, main = "")
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, xlab = "Rubin")
  )
  expect_no_error(
    qqplot_rubin_abpool(abpool.out.multi, pch=2)
  )
})
