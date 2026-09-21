#####
# mice - linear regression
set.seed(1)
n <- 100
X <- rnorm(n); Z <- rnorm(n)
Y <- 1+3*X + Z + rnorm(n)
X[1:50] <- NA
data.lin <- data.frame(Y=Y,X=X,Z=Z)
m <- 200
impute.mice <- mice::mice(data.lin, m = m, method = "norm", print=FALSE)
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
    confint(abpool.out.multi, parm = list(1,2))
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
###########################################
# Output
scalar.out <- confint(abpool.out.scalar)
multi.out <- confint(abpool.out.multi)
one.out <- confint(abpool.out.multi, parm = "X")
perc_names_95 <- c("2.5 %", "97.5 %")
perc_names_80 <- c("10 %", "90 %")
test_that("output has correct dimensions, type, names", {
  # type
  expect_true(is.matrix(scalar.out))
  expect_true(is.matrix(multi.out))
  expect_true(is.matrix(one.out))
  expect_true(is.numeric(scalar.out))
  expect_true(is.numeric(multi.out))
  expect_true(is.numeric(one.out))
  # Dimensions
  expect_equal(dim(scalar.out)[1],1)
  expect_equal(dim(multi.out)[1],3)
  expect_equal(dim(one.out)[1],1)
  expect_equal(dim(scalar.out)[2],2)
  expect_equal(dim(multi.out)[2],2)
  expect_equal(dim(one.out)[2],2)
  # colnames
  expect_equal(colnames(scalar.out),perc_names_95)
  expect_equal(colnames(multi.out),perc_names_95)
  expect_equal(colnames(one.out),perc_names_95)
  expect_equal(colnames(confint(abpool.out.scalar, level = 0.8)),perc_names_80)
  expect_equal(colnames(confint(abpool.out.multi, level = 0.8)),perc_names_80)
  expect_equal(colnames(confint(abpool.out.multi, parm = "X", level = 0.8)),perc_names_80)
  # rownames
  expect_equal(rownames(scalar.out),abpool.out.scalar$parameters)
  expect_equal(rownames(multi.out),abpool.out.multi$parameters)
  expect_equal(rownames(one.out),"X")
})
test_that("confint correctly applies quantile", {
  expect_equal(scalar.out,quantile(abpool.out.scalar$samples,c(0.025,0.975)), ignore_attr = TRUE) # Ignore names etc.
  expect_equal(one.out,quantile(abpool.out.multi$samples[,"X"],c(0.025,0.975)), ignore_attr = TRUE)
  expect_equal(multi.out[1,],quantile(abpool.out.multi$samples[,1],c(0.025,0.975)), ignore_attr = TRUE)
  expect_equal(multi.out[2,],quantile(abpool.out.multi$samples[,2],c(0.025,0.975)), ignore_attr = TRUE)
  expect_equal(multi.out[3,],quantile(abpool.out.multi$samples[,3],c(0.025,0.975)), ignore_attr = TRUE)

  expect_equal(confint(abpool.out.scalar, level = 0.8),quantile(abpool.out.scalar$samples,c(0.1,0.9)), ignore_attr = TRUE) # Ignore names etc.
  expect_equal(confint(abpool.out.multi, parm = "X", level = 0.8),quantile(abpool.out.multi$samples[,"X"],c(0.1,0.9)), ignore_attr = TRUE)
  expect_equal(confint(abpool.out.multi, level = 0.8)[1,],quantile(abpool.out.multi$samples[,1],c(0.1,0.9)), ignore_attr = TRUE)
  expect_equal(confint(abpool.out.multi, level = 0.8)[2,],quantile(abpool.out.multi$samples[,2],c(0.1,0.9)), ignore_attr = TRUE)
  expect_equal(confint(abpool.out.multi, level = 0.8)[3,],quantile(abpool.out.multi$samples[,3],c(0.1,0.9)), ignore_attr = TRUE)
})
test_that("parameter ordering outputs correctly", {
  # Selecting parameters in confint is same as subsetting after
  expect_equal(multi.out["X",],one.out, ignore_attr = TRUE)
  # Negative parameters chooses correctly
  expect_equal(rownames(confint(abpool.out.multi, parm=-c(1,3))),"X")
  expect_equal(confint(abpool.out.multi, parm=-c(1,3)),one.out)
  # Parameter reordering
  expect_equal(confint(abpool.out.multi, parm=c(3,2,1)),multi.out[c(3,2,1),])
})
