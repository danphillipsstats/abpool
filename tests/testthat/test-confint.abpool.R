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
    confint(abpool.out.scalar)
  )
  # scalar
  expect_no_error(
    confint.abpool(abpool.out.scalar)
  )
  # multi
  expect_no_error(
    confint(abpool.out.multi)
  )
  # multi
  expect_no_error(
    confint.abpool(abpool.out.multi)
  )
})
# parm
test_that("parm accepts valid inputs", {
  # scalar
  expect_no_error(
    confint(abpool.out.scalar)
  )
  expect_no_error(
    confint(abpool.out.scalar, parm = c(1))
  )
  expect_no_error(
    confint(abpool.out.scalar, parm = c("X"))
  )
  # multi
  expect_no_error(
    confint(abpool.out.multi)
  )
  expect_no_error(
    confint(abpool.out.multi, parm = c(1))
  )
  expect_no_error(
    confint(abpool.out.multi, parm = c(3,2))
  )
  expect_no_error(
    confint(abpool.out.multi, parm = c(-1))
  )
  expect_no_error(
    confint(abpool.out.multi, parm = "X")
  )
  expect_no_error(
    confint(abpool.out.multi, parm = c("Z","X"))
  )
})
test_that("parm rejects invalid inputs", {
  # scalar
  expect_error(
    confint(abpool.out.scalar, parm = 2)
  )
  expect_error(
    confint(abpool.out.scalar, parm = -1)
  )
  expect_error(
    confint(abpool.out.scalar, parm = NA)
  )
  expect_error(
    confint(abpool.out.scalar, parm = c("Z")) # "Z" was not in parameters input for abpool, although was a parameter in the model
  )
  # multi
  expect_error(
    confint(abpool.out.multi, parm = -c(1:3))
  )
  expect_error(
    confint(abpool.out.multi, parm = c(-1,3))
  )
  expect_error(
    confint(abpool.out.multi, parm = c("X","click"))
  )
  expect_error(
    confint(abpool.out.multi, parm = NA)
  )
})
test_that("level accepts valid inputs", {
  expect_no_error(
    confint(abpool.out.multi)
  )
  expect_no_error(
    confint(abpool.out.multi, level = 0.5)
  )
})
test_that("level rejects invalid inputs", {
  # multi
  expect_error(
    confint(abpool.out.multi, level = -1)
  )
  expect_error(
    confint(abpool.out.multi, level = 2)
  )
  expect_error(
    confint(abpool.out.multi, level = c(0.9,0.7))
  )
  expect_error(
    confint(abpool.out.multi, level = NA)
  )
  expect_error(
    confint(abpool.out.multi, level = 0)
  )
  expect_error(
    confint(abpool.out.multi, level = 1)
  )
  expect_error(
    confint(abpool.out.multi, level = "95%")
  )
})
