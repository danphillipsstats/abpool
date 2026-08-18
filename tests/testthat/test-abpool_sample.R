valid_scalar_estimates <- 1:3
valid_scalar_variances <- 1:3
valid_df <- Inf
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
