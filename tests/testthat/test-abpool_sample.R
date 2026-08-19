valid_scalar_estimates <- 1:3
valid_scalar_variances <- 1:3
valid_df <- Inf
##################################################
# Input validation
################
# df
test_that("df accepts valid values", {
  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df)
  )

  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=0.1)
  )

  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=10)
  )
})
test_that("df rejects invalid values", {
  # 0
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=0)
  )
  # Negative
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=-1)
  )
  # NA
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=NA)
  )
  # NULL
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances)
  )
  # Negative Inf
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=-Inf)
  )
  # character
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df="1")
  )
  # vector
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df=1:3)
  )
  # list
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,df = list(1,2,3))
  )
})
######################
# J
test_that("J accepts valid values", {
  # Not specified
  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df)
  )
  # J = 1
  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=1)
  )
  # other integer
  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=2)
  )
})
test_that("J rejects invalid values", {
  # 0
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=0)
  )
  # Non-integer
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=1.5)
  )
  # Negative
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=-1)
  )
  # NA
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=NA)
  )
  # NULL
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=NULL)
  )
  # NaN
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=NaN)
  )
  # Inf
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=Inf)
  )
  # character
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J="2")
  )
  # vector
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=1:3)
  )
  # list
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df,J=list(1,2,3))
  )
})
######################
# Scalar case
#####
# m
test_that("m accepts valid values", {
  # m = 1
  expect_no_error(
    abpool_sample(
      estimates = 1,
      variances = 1,
      valid_df
    )
  )
  # m = 100
  expect_no_error(
    abpool_sample(
      estimates = 1:100,
      variances = 1:100,
      valid_df
    )
  )
})
test_that("m rejects invalid values", {
  # 0
  expect_error(
    abpool_sample(
      estimates = numeric(0),
      variances = numeric(0),
      valid_df
    )
  )
  # estimates length 0
  expect_error(
    abpool_sample(
      estimates = 1:10,
      variances = numeric(0),
      valid_df
    )
  )
  # length estimates not equal to length variances
  expect_error(
    abpool_sample(
      estimates = 1:10,
      variances = 1:3,
      valid_df
    )
  )
})
#####
# estimates
test_that("estimates accepts valid values", {
  # 0
  expect_no_error(
    abpool_sample(estimates = c(1,2,0), variances = 1:3, df = valid_df)
  )
  # negative estimate is valid
  expect_no_error(
    abpool_sample(estimates = c(1,2,-10), variances = 1:3, df = valid_df)
  )
})
test_that("estimates rejects invalid values", {
  # NA
  expect_error(
    abpool_sample(estimates = c(1,2,NA), variances = 1:3, df = valid_df)
  )
  # NaN
  expect_error(
    abpool_sample(estimates = c(1,2,NaN), variances = 1:3, df = valid_df)
  )
  # NULL
  expect_error(
    abpool_sample(estimates = NULL, variances = 1:3, df = valid_df)
  )
  # Infinite
  expect_error(
    abpool_sample(estimates = c(1,2,Inf), variances = 1:3, df = valid_df)
  )
  # matrix
  expect_error(
    abpool_sample(estimates = matrix(1:4,ncol=2), variances = 1:4, df = valid_df)
  )
})
#####
# variances
test_that("variances accepts valid values", {
  # 0
  expect_no_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,0), df = valid_df)
  )
})
test_that("variances rejects invalid values", {
  # negative
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,-10), df = valid_df)
  )
  # NA
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,NA), df = valid_df)
  )
  # NaN
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,NaN), df = valid_df)
  )
  # NULL
  expect_error(
    abpool_sample(estimates = 1:3, variances = NULL, df = valid_df)
  )
  # Infinite
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,Inf), df = valid_df)
  )
  # matrix
  expect_error(
    abpool_sample(estimates = 1:4, variances = matrix(1:4,ncol=2), df = valid_df)
  )
})
########
# Multi-parameter case
##################################################
# Output validation
######
# Scalar case
######
# test length
test_that("scalar output has correct length for J = 1", {
  # test length df finite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = 10
  )
  expect_length(samples, length(valid_scalar_estimates))
  # test length df infinite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = Inf
  )
  expect_length(samples, length(valid_scalar_estimates))
  # test length df finite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    df = 10
  )
  expect_length(samples, length(1))
  # test length df infinite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    df = Inf
  )
  expect_length(samples, length(1))
})
# test length for J > 1
test_that("scalar output has correct length for J > 1", {
  # test length df finite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = 10,
    J = 2
  )
  expect_length(samples, length(rep(valid_scalar_estimates,2)))
  # test length df infinite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = Inf,
    J = 2
  )
  expect_length(samples, length(rep(valid_scalar_estimates,2)))
  # test length df finite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    df = 10,
    J = 2
  )
  expect_length(samples, length(rep(1,2)))
  # test length df infinite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    df = Inf,
    J = 2
  )
  expect_length(samples, length(rep(1,2)))
})
# test type
test_that("scalar output has correct type", {
  # test type df finite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = 10
  )
  expect_type(samples, "double")
  # test type df infinite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = Inf
  )
  expect_type(samples, "double")
  # test type df finite type 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    df = 10
  )
  expect_type(samples, "double")
  # test type df infinite type 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    df = Inf
  )
  expect_type(samples, "double")
})
#####
# J = 1
test_that("zero variance returns estimates exactly", {
  samples <- abpool_sample(
    estimates = c(-2, 0, 4),
    variances = c(0, 0, 0),
    df = 10
  )
  expect_equal(samples, c(-2, 0, 4))
})
test_that("output matches under set.seed", {
  # Check abpool_sample gives same answer as rnorm df=Inf
  set.seed(123)
  actual <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = Inf
  )
  set.seed(123)
  expected <- valid_scalar_estimates + rnorm(3) * sqrt(valid_scalar_variances)
  expect_equal(actual, expected)
  # Check abpool_sample gives same answer as rt for df finite
  set.seed(123)
  actual <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    df = 10
  )
  set.seed(123)
  expected <- valid_scalar_estimates + rt(3,df=10) * sqrt(valid_scalar_variances)
  expect_equal(actual, expected)
})
#####
# J > 1
test_that("J > 1 gives correct order", {
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = rep(0,3),
    df = 10,
    J = 2
  )

  expect_equal(samples, rep(valid_scalar_estimates,each=2))
})
