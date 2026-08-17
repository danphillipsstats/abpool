################
# df
test_that("df accepts valid values", {
  expect_no_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10
    )
  )

  expect_no_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 0.1
    )
  )

  expect_no_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = Inf
    )
  )
})
test_that("df rejects invalid values", {
  # 0
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 0
    )
  )
  # Negative
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = -1
    )
  )
  # NA
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = NA
    )
  )
  # NULL
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3
    )
  )
  # Negative Inf
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = -Inf
    )
  )
  # character
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = "test"
    )
  )
  # vector
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 1:3
    )
  )
  # list
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = list(1,2,3)
    )
  )
})
######################
# J
test_that("J accepts valid values", {
  # Not specified
  expect_no_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10
    )
  )
  # J = 1
  expect_no_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = 1
    )
  )
  # other integer
  expect_no_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = 2
    )
  )
})
test_that("J rejects invalid values", {
  # 0
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = 0
    )
  )
  # Non-integer
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = 1.5
    )
  )
  # Negative
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = -1
    )
  )
  # NA
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = NA
    )
  )
  # NULL
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = NULL
    )
  )
  # NaN
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = NaN
    )
  )
  # Inf
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = Inf
    )
  )
  # character
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = "2"
    )
  )
  # vector
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = 1:3
    )
  )
  # list
  expect_error(
    abpool_sample(
      estimates = 1:3,
      variances = 1:3,
      df = 10,
      J = list(1,2,3)
    )
  )
})
