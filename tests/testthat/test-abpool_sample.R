valid_scalar_estimates <- 1:3
valid_scalar_variances <- 1:3
valid_df <- Inf
valid_multi_estimates <- list(c(x = 1, z = 2), c(x = 3, z = 4), c(x = 5, z = 6) )
variance_mat <- matrix(c(1,0.1,0.1,2),2,2)
valid_multi_variances <- list(variance_mat, variance_mat, variance_mat)
##################################################
# Input validation
################
# dfcom
test_that("dfcom accepts valid values", {
  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,valid_df)
  )

  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=0.1)
  )

  expect_no_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=10)
  )
})
test_that("dfcom rejects invalid values", {
  # 0
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=0)
  )
  # Negative
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=-1)
  )
  # NA
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=NA)
  )
  # NaN
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=NaN)
  )
  # NULL
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances)
  )
  # Negative Inf
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=-Inf)
  )
  # character
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom="1")
  )
  # vector
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom=1:3)
  )
  # list
  expect_error(
    abpool_sample(valid_scalar_estimates,valid_scalar_variances,dfcom = list(1,2,3))
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
test_that("m accepts valid values - scalar case", {
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
test_that("m accepts valid values - multivariate case", {
  # m = 2
  expect_no_error(
    abpool_sample(
      estimates = list(c(1,2),c(1,2)),
      variances = list(diag(c(1,1)),diag(c(1,1))),
      dfcom = valid_df
    )
  )
  # m = 1
  expect_no_error(
    abpool_sample(
      estimates = list(c(1,2)),
      variances = list(diag(c(1,1))),
      dfcom = valid_df
    )
  )
})
test_that("m rejects invalid values - scalar case", {
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
test_that("m rejects invalid values - multivariate case", {
  # 0
  expect_error(
    abpool_sample(
      estimates = list(),
      variances = list(),
      valid_df
    )
  )
  # estimates length 0
  expect_error(
    abpool_sample(
      estimates = list(),
      variances = valid_multi_variances,
      valid_df
    )
  )
  # length estimates not equal to length variances
  expect_error(
    abpool_sample(
      estimates = valid_multi_estimates,
      variances = list(variance_mat,variance_mat),
      valid_df
    )
  )
})

#####
# estimates
test_that("estimates accepts valid values", {
  # 0
  expect_no_error(
    abpool_sample(estimates = c(1,2,0), variances = 1:3, dfcom = valid_df)
  )
  # negative estimate is valid
  expect_no_error(
    abpool_sample(estimates = c(1,2,-10), variances = 1:3, dfcom = valid_df)
  )
})
test_that("estimates rejects invalid values", {
  # NA
  expect_error(
    abpool_sample(estimates = c(1,2,NA), variances = 1:3, dfcom = valid_df)
  )
  # NaN
  expect_error(
    abpool_sample(estimates = c(1,2,NaN), variances = 1:3, dfcom = valid_df)
  )
  # NULL
  expect_error(
    abpool_sample(estimates = NULL, variances = 1:3, dfcom = valid_df)
  )
  # Infinite
  expect_error(
    abpool_sample(estimates = c(1,2,Inf), variances = 1:3, dfcom = valid_df)
  )
  # matrix
  expect_error(
    abpool_sample(estimates = matrix(1:4,ncol=2), variances = 1:4, dfcom = valid_df)
  )
  # non-numeric estimates
  expect_error(
    abpool_sample(estimates = c(1,"2",3), variances = 1:3, dfcom = valid_df)
  )
  # non-numeric variances
  expect_error(
    abpool_sample(estimates = c(1,2,3), variances = c(1,"2",3), dfcom = valid_df)
  )
})
#####
# variances
test_that("variances accepts valid values", {
  # 0
  expect_no_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,0), dfcom = valid_df)
  )
})
test_that("variances rejects invalid values", {
  # negative
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,-10), dfcom = valid_df)
  )
  # NA
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,NA), dfcom = valid_df)
  )
  # NaN
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,NaN), dfcom = valid_df)
  )
  # NULL
  expect_error(
    abpool_sample(estimates = 1:3, variances = NULL, dfcom = valid_df)
  )
  # Infinite
  expect_error(
    abpool_sample(estimates = 1:3, variances = c(1,2,Inf), dfcom = valid_df)
  )
  # matrix
  expect_error(
    abpool_sample(estimates = 1:4, variances = matrix(1:4,ncol=2), dfcom = valid_df)
  )
})
########
# Multi-parameter case
test_that("estimates and variances accept valid values", {
  # standard case
  expect_no_error(
    abpool_sample(valid_multi_estimates, valid_multi_variances, dfcom = valid_df)
  )
  expect_no_error(
    abpool_sample(valid_multi_estimates, valid_multi_variances, dfcom = Inf)
  )
  expect_no_error(
    abpool_sample(valid_multi_estimates, valid_multi_variances, dfcom = valid_df, J = 2)
  )
  expect_no_error(
    abpool_sample(valid_multi_estimates, valid_multi_variances, dfcom = Inf, J = 2)
  )
  # p = 1 case
  expect_no_error(
    abpool_sample(estimates = list(c(1),c(2),c(3)), variances = list(matrix(1,1,1),matrix(1,1,1),matrix(1, 1, 1)), dfcom = valid_df)
  )
  expect_no_error(
    abpool_sample(estimates = list(c(1),c(2),c(3)), variances = list(matrix(1,1,1),matrix(1,1,1),matrix(1, 1, 1)), dfcom = Inf)
  )
  expect_no_error(
    abpool_sample(estimates = list(c(1),c(2),c(3)), variances = list(matrix(1,1,1),matrix(1,1,1),matrix(1, 1, 1)), dfcom = valid_df, J = 2)
  )
  expect_no_error(
    abpool_sample(estimates = list(c(1),c(2),c(3)), variances = list(matrix(1,1,1),matrix(1,1,1),matrix(1, 1, 1)), dfcom = Inf, J = 2)
  )
})
test_that("estimates rejects invalid values", {
  # Different p for different elements of estimates
  expect_error(
    abpool_sample(estimates = list(c(1,2),c(1,2,3),c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
  # all entries null
  expect_error(
    abpool_sample(estimates = list(NULL,NULL,NULL), variances = list(NULL,NULL,NULL), dfcom = valid_df)
  )
  # NULL entry
  expect_error(
    abpool_sample(estimates = list(c(1,2),NULL,c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
  # NA entry
  expect_error(
    abpool_sample(estimates = list(c(1,2),c(1,NA),c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
  # NaN entry
  expect_error(
    abpool_sample(estimates = list(c(1,2),c(1,NaN),c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
  # Inf entry
  expect_error(
    abpool_sample(estimates = list(c(1,2),c(1,Inf),c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
  # Matrix entry
  expect_error(
    abpool_sample(estimates = list(c(1,2),matrix(1:2,ncol=2),c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
  # Non-numeric entry
  expect_error(
    abpool_sample(estimates = list(c(1,2),c(1,"2"),c(1,2)), valid_multi_variances, dfcom = valid_df)
  )
})
test_that("variances accepts valid values", {
  # Positive definite
  expect_no_error(
    abpool_sample(valid_multi_estimates, list(variance_mat,matrix(c(1,0,0,2),2,2),variance_mat), dfcom = valid_df)
  )
  # Singular positive semi-definite
  expect_no_error(
    abpool_sample(valid_multi_estimates, list(variance_mat,matrix(c(1, 1, 1, 1),2),variance_mat), dfcom = valid_df)
  )
})

test_that("variances rejects invalid values", {
  # Different p for different elements of variances
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,matrix(c(1,0.1,0.1,0.1,1,0.1,0.1,0.1,2),3,3),variance_mat), dfcom = valid_df)
  )
  # NULL entry
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,NULL,variance_mat), dfcom = valid_df)
  )
  # NA entry
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,matrix(c(1,NA,0,2)),variance_mat), dfcom = valid_df)
  )
  # NaN entry
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,matrix(c(1,NaN,0,2)),variance_mat), dfcom = valid_df)
  )
  # Inf entry
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,matrix(c(Inf,0,0,2)),variance_mat), dfcom = valid_df)
  )
  # Vector entry
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,c(1,0,0,2),variance_mat), dfcom = valid_df)
  )
  # Non-numeric entry
  expect_error(
    abpool_sample(valid_multi_estimates, variances = list(variance_mat,matrix(c(1,0,0,"2")),variance_mat), dfcom = valid_df)
  )
  # Non-square
  expect_error(
    abpool_sample(valid_multi_estimates,
                  variances = list(matrix(c(1,0.1,0.1,0.1,0.1,2),2),matrix(c(1,0.1,0.1,0.1,0.1,2),2),matrix(c(1,0.1,0.1,0.1,0.1,2),2)),
                  dfcom = valid_df)
  )
  # Non-symmetric
  expect_error(
    abpool_sample(valid_multi_estimates,
                  variances = list(variance_mat,matrix(c(1,0.1,0,2),2),variance_mat),
                  dfcom = valid_df)
  )
  # Symmetric indefinite
  expect_error(
    abpool_sample(valid_multi_estimates, list(variance_mat,matrix(c(1, 2, 2, 1),2),variance_mat), dfcom = valid_df)
  )
})

test_that("estimates and variances reject incompatible values", {
  # Dimensions of estimates and variances don't match
  expect_error(
    abpool_sample(
      estimates = list(c(1,2,3),c(1,2,3),c(1,2,3)),
      valid_multi_variances,
      dfcom = valid_df)
  )
})
##################################################
# Output validation
######################
# Scalar case
######
# test length
test_that("scalar output has correct length for J = 1", {
  # test length dfcom finite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = 10
  )
  expect_length(samples, length(valid_scalar_estimates))
  # test length dfcom infinite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = Inf
  )
  expect_length(samples, length(valid_scalar_estimates))
  # test length dfcom finite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    dfcom = 10
  )
  expect_length(samples, length(1))
  # test length dfcom infinite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    dfcom = Inf
  )
  expect_length(samples, length(1))
})
# test length for J > 1
test_that("scalar output has correct length for J > 1", {
  # test length dfcom finite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = 10,
    J = 2
  )
  expect_length(samples, length(rep(valid_scalar_estimates,2)))
  # test length dfcom infinite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = Inf,
    J = 2
  )
  expect_length(samples, length(rep(valid_scalar_estimates,2)))
  # test length dfcom finite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    dfcom = 10,
    J = 2
  )
  expect_length(samples, length(rep(1,2)))
  # test length dfcom infinite length 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    dfcom = Inf,
    J = 2
  )
  expect_length(samples, length(rep(1,2)))
})
# test type
test_that("scalar output has correct type and is finite", {
  # test type dfcom finite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = 10
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom infinite
  samples <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = Inf
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom finite type 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    dfcom = 10
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom infinite type 1
  samples <- abpool_sample(
    estimates = 1,
    variances = 1,
    dfcom = Inf
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
})
#####
# J = 1
test_that("zero variance returns estimates exactly", {
  samples <- abpool_sample(
    estimates = c(-2, 0, 4),
    variances = c(0, 0, 0),
    dfcom = 10
  )
  expect_equal(samples, c(-2, 0, 4))
})
test_that("output matches under set.seed", {
  # Check abpool_sample gives same answer as rnorm dfcom=Inf
  set.seed(123)
  actual <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = Inf
  )
  set.seed(123)
  expected <- valid_scalar_estimates + rnorm(3) * sqrt(valid_scalar_variances)
  expect_equal(actual, expected)
  # Check abpool_sample gives same answer as rt for dfcom finite
  set.seed(123)
  actual <- abpool_sample(
    estimates = valid_scalar_estimates,
    variances = valid_scalar_variances,
    dfcom = 10
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
    dfcom = 10,
    J = 2
  )
  expect_equal(samples, rep(valid_scalar_estimates,each=2))
})
test_that("zero variance returns estimates exactly", {
  samples <- abpool_sample(
    estimates = c(-2, 0, 4),
    variances = c(0, 0, 0),
    dfcom = 10,
    J = 2
  )
  expect_equal(samples, rep(c(-2, 0, 4),each=2))
})
######################
# Multivariate case
#######
# test type
test_that("multivariate output has correct type and is finite", {
  # test type dfcom finite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = 10
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom infinite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = Inf
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom finite type m = 1
  samples <- abpool_sample(
    estimates = list(c(1,2)),
    variances = list(variance_mat),
    dfcom = 10
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom infinite type m = 1
  samples <- abpool_sample(
    estimates = list(c(1,2)),
    variances = list(variance_mat),
    dfcom = Inf
  )
  expect_type(samples, "double")
  # test type dfcom finite J > 1
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = 10,
    J = 2
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
  # test type dfcom infinite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = Inf,
    J = 2
  )
  expect_type(samples, "double")
  expect_true(all(is.finite(samples)))
})
#######
# test dimensions
test_that("multivariate output has correct dimensions for J = 1", {
  # test dims dfcom finite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = 10
  )
  expect_equal(dim(samples), c(length(valid_multi_estimates),length(valid_multi_estimates[[1]])))
  # test dims dfcom infinite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = Inf
  )
  expect_equal(dim(samples), c(length(valid_multi_estimates),length(valid_multi_estimates[[1]])))
  # test dims dfcom finite length 1
  samples <- abpool_sample(
    estimates = list(c(1,2)),
    variances = list(variance_mat),
    dfcom = 10
  )
  expect_equal(dim(samples), c(length(list(c(1,2))),length(list(c(1,2))[[1]])))
  # test dims dfcom infinite length 1
  samples <- abpool_sample(
    estimates = list(c(1,2)),
    variances = list(variance_mat),
    dfcom = Inf
  )
  expect_equal(dim(samples), c(length(list(c(1,2))),length(list(c(1,2))[[1]])))
})
test_that("multivariate output has correct dimensions for J > 1", {
  # test dims dfcom finite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = 10,
    J = 2
  )
  expect_equal(dim(samples), c(length(valid_multi_estimates)*2,length(valid_multi_estimates[[1]])))
  # test dims dfcom infinite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = Inf,
    J = 2
  )
  expect_equal(dim(samples), c(length(valid_multi_estimates)*2,length(valid_multi_estimates[[1]])))
  # test dims dfcom finite length 1
  samples <- abpool_sample(
    estimates = list(c(1,2)),
    variances = list(variance_mat),
    dfcom = 10,
    J = 2
  )
  expect_equal(dim(samples), c(length(list(c(1,2)))*2,length(list(c(1,2))[[1]])))
  # test dims dfcom infinite length 1
  samples <- abpool_sample(
    estimates = list(c(1,2)),
    variances = list(variance_mat),
    dfcom = Inf,
    J = 2
  )
  expect_equal(dim(samples), c(length(list(c(1,2)))*2,length(list(c(1,2))[[1]])))
})
#######
# zero covariance
zero_cov_mat <- matrix(0, 2, 2)
zero_multi_variances <- list(zero_cov_mat,zero_cov_mat,zero_cov_mat)
test_that("multivariate output is correct, and rows ordered correctly, when covariance matrix 0, J = 1", {
  # dfcom finite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = zero_multi_variances,
    dfcom = 10
  )
  expect_equal(samples, t(vapply(valid_multi_estimates,c,numeric(2))))
  # dfcom infinite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = zero_multi_variances,
    dfcom = Inf
  )
  expect_equal(samples, t(vapply(valid_multi_estimates,c,numeric(2))))
})
test_that("multivariate output is correct, and rows ordered correctly, when covariance matrix 0, J > 1", {
  # dfcom finite, J > 1
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = zero_multi_variances,
    dfcom = 10,
    J = 2
  )
  expect_equal(samples, t(vapply(rep(valid_multi_estimates,each=2),c,numeric(2))))
  # dfcom infinite, J > 1
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = zero_multi_variances,
    dfcom = Inf,
    J = 2
  )
  expect_equal(samples, t(vapply(rep(valid_multi_estimates,each=2),c,numeric(2))))
})
#######
# Perfect correlation of -1
zero_multi_estimates <- list(c(x = 0, z = 0), c(x = 0, z = 0), c(x = 0, z = 0) )
perfect_correlation_mat <- matrix(c(1,-1,-1,1), 2, 2)
corr_multi_variances <- list(perfect_correlation_mat,perfect_correlation_mat,perfect_correlation_mat)
test_that("multivariate output captures complete correlation, J = 1", {
  # dfcom finite
  samples <- abpool_sample(
    estimates = zero_multi_estimates,
    variances = corr_multi_variances,
    dfcom = 10
  )
  expect_equal(samples[,1], -samples[,2])
  # dfcom infinite
  samples <- abpool_sample(
    estimates = zero_multi_estimates,
    variances = corr_multi_variances,
    dfcom = Inf
  )
  expect_equal(samples[,1], -samples[,2])
})
test_that("multivariate output captures complete correlation, J > 1", {
  # dfcom finite, J > 1
  samples <- abpool_sample(
    estimates = zero_multi_estimates,
    variances = corr_multi_variances,
    dfcom = 10,
    J = 2
  )
  expect_equal(samples[,1], -samples[,2])
  # dfcom infinite, J > 1
  samples <- abpool_sample(
    estimates = zero_multi_estimates,
    variances = corr_multi_variances,
    dfcom = Inf,
    J = 2
  )
  expect_equal(samples[,1], -samples[,2])
})
#########
# Multivariate column names
test_that("column names are retained in multivariate case", {
  # column names dfcom finite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = 10
  )
  expect_equal(colnames(samples),names(valid_multi_estimates[[1]]))
  # column names dfcom infinite
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = Inf
  )
  expect_equal(colnames(samples),names(valid_multi_estimates[[1]]))
  # column names dfcom finite, J > 1
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = 10,
    J = 2
  )
  expect_equal(colnames(samples),names(valid_multi_estimates[[1]]))
  # column names dfcom infinite, J > 1
  samples <- abpool_sample(
    estimates = valid_multi_estimates,
    variances = valid_multi_variances,
    dfcom = Inf,
    J = 2
  )
  expect_equal(colnames(samples),names(valid_multi_estimates[[1]]))
  # null column names dfcom finite
  samples <- abpool_sample(
    estimates = list(c(1,2),c(1,2),c(1,2)),
    variances = valid_multi_variances,
    dfcom = 10
  )
  expect_null(colnames(samples))
  # null column names dfcom infinite
  samples <- abpool_sample(
    estimates = list(c(1,2),c(1,2),c(1,2)),
    variances = valid_multi_variances,
    dfcom = Inf
  )
  expect_null(colnames(samples))
  # null column names dfcom finite, J > 1
  samples <- abpool_sample(
    estimates = list(c(1,2),c(1,2),c(1,2)),
    variances = valid_multi_variances,
    dfcom = 10,
    J = 2
  )
  expect_null(colnames(samples))
  # null column names dfcom infinite, J > 1
  samples <- abpool_sample(
    estimates = list(c(1,2),c(1,2),c(1,2)),
    variances = valid_multi_variances,
    dfcom = Inf,
    J = 2
  )
  expect_null(colnames(samples))
})
########################
# Neither scalar nor multivariate
test_that("mixed scalar/multivariate input gives error", {
  # scalar estimates, multivariate variances
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = valid_multi_variances,
      dfcom = valid_df
    )
  )
  expect_error(
    abpool_sample(
    estimates = valid_multi_estimates,
    variances = 1:3,
    dfcom = valid_df
    )
  )
})
