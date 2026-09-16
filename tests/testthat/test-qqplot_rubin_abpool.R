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
###############################################################################
# Plotting
test_that("qqplot_rubin_abpool passes coordinates to qqplot (scalar case)", {
  qqplot_args <- NULL

  local_mocked_bindings(
    qqplot = function(...) {
      qqplot_args <<- list(...)
    },
    .package = "stats"
  )

  result <- qqplot_rubin_abpool(abpool.out.scalar)

  expect_true(!is.null(qqplot_args))
  expect_equal(qqplot_args$x, result$x)
  expect_equal(qqplot_args$y, result$y)
})
test_that("qqplot_rubin_abpool passes coordinates to qqplot for multiple parameters", {
  qqplot_args <- list()

  local_mocked_bindings(
    qqplot = function(...) {
      qqplot_args[[length(qqplot_args) + 1]] <<- list(...)
    },
    .package = "stats"
  )

  result <- qqplot_rubin_abpool(abpool.out.multi)

  # One qqplot call per parameter
  expect_length(qqplot_args, length(result))

  for (i in seq_along(result)) {
    expect_equal(qqplot_args[[i]]$x, result[[i]]$x)
    expect_equal(qqplot_args[[i]]$y, result[[i]]$y)
    expect_equal(qqplot_args[[i]]$main, paste("Q-Q plot:",abpool.out.multi$parameters[i])) # Title
  }
})
test_that("plot.it determines whether qqplot is called (scalar)", {
  # FALSE
  n_qqplot <- 0L

  local_mocked_bindings(
    qqplot = function(...) {
      n_qqplot <<- n_qqplot + 1L
    },
    .package = "stats"
  )

  qqplot_rubin_abpool(abpool.out.scalar, plot.it = FALSE)

  expect_equal(n_qqplot, 0L)

  # TRUE
  n_qqplot <- 0L
  qqplot_rubin_abpool(abpool.out.scalar, plot.it = TRUE)
  expect_equal(n_qqplot, length(abpool.out.scalar$parameters))
})
test_that("plot.it determines whether qqplot is called (multivariate)", {
  # FALSE
  n_qqplot <- 0L

  local_mocked_bindings(
    qqplot = function(...) {
      n_qqplot <<- n_qqplot + 1L
    },
    .package = "stats"
  )

  qqplot_rubin_abpool(abpool.out.multi, plot.it = FALSE)

  expect_equal(n_qqplot, 0L)

  # TRUE
  n_qqplot <- 0L
  qqplot_rubin_abpool(abpool.out.multi, plot.it = TRUE)
  expect_equal(n_qqplot, length(abpool.out.multi$parameters))
})
# parm
test_that("parm determines the number of plots", {
  # supplied
  n_qqplot <- 0L

  local_mocked_bindings(
    qqplot = function(...) {
      n_qqplot <<- n_qqplot + 1L
    },
    .package = "stats"
  )
  parm_in <- c("X","Z")
  qqplot_rubin_abpool(abpool.out.multi, parm = parm_in)

  expect_equal(n_qqplot, length(parm_in))
})
# qqline
test_that("add_qqline determines whether qqline is called (scalar)", {
  # FALSE
  n_qqline <- 0L

  local_mocked_bindings(
    qqline = function(...) {
      n_qqline <<- n_qqline + 1L
    },
    .package = "stats"
  )

  qqplot_rubin_abpool(abpool.out.scalar, add_qqline = FALSE)

  expect_equal(n_qqline, 0L)

  # TRUE
  n_qqline <- 0L
  qqplot_rubin_abpool(abpool.out.scalar, add_qqline = TRUE)
  expect_equal(n_qqline, length(abpool.out.scalar$parameters))
})
test_that("add_qqline determines whether qqline is called (multivariate)", {
  # FALSE
  n_qqline <- 0L

  local_mocked_bindings(
    qqline = function(...) {
      n_qqline <<- n_qqline + 1L
    },
    .package = "stats"
  )

  qqplot_rubin_abpool(abpool.out.multi, add_qqline = FALSE)

  expect_equal(n_qqline, 0L)

  # TRUE
  n_qqline <- 0L
  qqplot_rubin_abpool(abpool.out.multi, add_qqline = TRUE)
  expect_equal(n_qqline, length(abpool.out.multi$parameters))
})
test_that("qqplot_rubin_abpool passes coordinates to qqline (scalar case)", {
  qqline_args <- NULL

  local_mocked_bindings(
    qqline = function(...) {
      qqline_args <<- list(...)
    },
    .package = "stats"
  )

  result <- qqplot_rubin_abpool(abpool.out.scalar)

  expect_true(!is.null(qqline_args))
  expect_equal(qqline_args$y, result$y)
})
test_that("qqplot_rubin_abpool passes coordinates to qqline for multiple parameters", {
  qqline_args <- list()

  local_mocked_bindings(
    qqline = function(...) {
      qqline_args[[length(qqline_args) + 1]] <<- list(...)
    },
    .package = "stats"
  )

  result <- qqplot_rubin_abpool(abpool.out.multi)

  # One qqline call per parameter
  expect_length(qqline_args, length(result))
  for (i in seq_along(result)) {
    expect_equal(qqline_args[[i]]$y, result[[i]]$y)
  }
})
test_that("qqplot_rubin_abpool passes graphical parameters to qqplot (scalar case)", {
  qqplot_args <- NULL

  local_mocked_bindings(
    qqplot = function(...) {
      qqplot_args <<- list(...)
    },
    .package = "stats"
  )

  result <- qqplot_rubin_abpool(abpool.out.scalar, main = "main", xlab = "xlab", ylab = "ylab", pch = 10)

  expect_equal(qqplot_args$main, "main")
  expect_equal(qqplot_args$xlab, "xlab")
  expect_equal(qqplot_args$ylab, "ylab")
  expect_equal(qqplot_args$pch, 10)
})
test_that("qqplot_rubin_abpool passes graphical parameters to qqplot for multiple parameters", {
  qqplot_args <- list()

  local_mocked_bindings(
    qqplot = function(...) {
      qqplot_args[[length(qqplot_args) + 1]] <<- list(...)
    },
    .package = "stats"
  )

  result <- qqplot_rubin_abpool(abpool.out.multi, main = "main", xlab = "xlab", ylab = "ylab", pch = 10)

  # One qqplot call per parameter
  expect_length(qqplot_args, length(result))
  for (i in seq_along(result)) {
    expect_equal(qqplot_args[[i]]$main, "main")
    expect_equal(qqplot_args[[i]]$xlab, "xlab")
    expect_equal(qqplot_args[[i]]$ylab, "ylab")
    expect_equal(qqplot_args[[i]]$pch, 10)
  }
})
############################################################################
# Outputs
qqplot.scalar <- qqplot_rubin_abpool(abpool.out.scalar)
qqplot.multi <- qqplot_rubin_abpool(abpool.out.multi)
qqplot.one <- qqplot_rubin_abpool(abpool.out.multi, parm = "X")
qqplot.reord <- qqplot_rubin_abpool(abpool.out.multi, parm = c("Z","X"))
samples.scalar <- abpool.out.scalar$samples
ordered.samples.scalar <- samples.scalar[order(samples.scalar)]
samples.multi <- abpool.out.multi$samples
ordered.samples.multi <- lapply(seq_len(ncol(samples.multi)), function(i) samples.multi[,i][order(samples.multi[,i])])
names(ordered.samples.multi) <- abpool.out.multi$parameters
n.samples.scalar <- length(ordered.samples.scalar)
n.samples.multi <- nrow(samples.multi)
test_that("qqplot_rubin_abpool returns output invisibly", {
  expect_invisible(
    qqplot_rubin_abpool(abpool.out.scalar)
  )
  expect_invisible(
    qqplot_rubin_abpool(abpool.out.multi)
  )
})
# output correct
test_that("output has correct structure", {
  # Scalar
  expect_true(is.list(qqplot.scalar))
  expect_true(names(qqplot.scalar)[1] == "x")
  expect_true(names(qqplot.scalar)[2] == "y")
  expect_true(names(qqplot.scalar)[3] == "p")
  expect_true(all.equal(unname(lengths(qqplot.scalar)),rep(abpool.out.scalar$m,3)))

  # Multivariate
  expect_true(is.list(qqplot.multi))
  expect_equal(names(qqplot.multi),abpool.out.multi$parameters)
  for (i in 1:length(qqplot.multi)){
    expect_true(is.list(qqplot.multi[[i]]))
    expect_true(names(qqplot.multi[[i]])[1] == "x")
    expect_true(names(qqplot.multi[[i]])[2] == "y")
    expect_true(names(qqplot.multi[[i]])[3] == "p")
    expect_true(all.equal(unname(lengths(qqplot.multi[[i]])),rep(abpool.out.multi$m,3)))
  }
  # One parameter
  expect_true(is.list(qqplot.one))
  expect_true(names(qqplot.one)[1] == "x")
  expect_true(names(qqplot.one)[2] == "y")
  expect_true(names(qqplot.one)[3] == "p")
  expect_true(all.equal(unname(lengths(qqplot.one)),rep(abpool.out.multi$m,3)))
})

# parm defines list structure correctly
test_that("parm defines list structure correctly", {
  expect_true(is.list(qqplot.reord))
  expect_equal(names(qqplot.reord),c("Z","X"))
  expect_equal(qqplot.reord[[1]]$y,ordered.samples.multi[["Z"]])
  expect_equal(qqplot.reord[[2]]$y,ordered.samples.multi[["X"]])
})
# y - ABpool samples
test_that("y is ordered samples", {
  # scalar
  expect_equal(qqplot.scalar$y,ordered.samples.scalar)
  # multi
  for (i in 1:length(qqplot.multi)){
    expect_equal(qqplot.multi[[i]]$y,ordered.samples.multi[[i]])
  }
  # scalar
  expect_equal(qqplot.one$y,ordered.samples.multi[["X"]])
})
# p - probabilities
test_that("p is correct ppoint() output", {
  # scalar
  expect_equal(qqplot.scalar$p,ppoints(n.samples.scalar))
  # multi
  for (i in 1:length(qqplot.multi)){
    expect_equal(qqplot.multi[[i]]$p,ppoints(n.samples.multi))
  }
  # scalar
  expect_equal(qqplot.one$p,ppoints(n.samples.multi))
})

# plot.it and add_qqline don't affect output
test_that("plot.it and add_qqline don't affect output", {
  # scalar
  expect_equal(qqplot.scalar,qqplot_rubin_abpool(abpool.out.scalar, plot.it = FALSE))
  expect_equal(qqplot.scalar,qqplot_rubin_abpool(abpool.out.scalar, plot.it = TRUE, add_qqline = FALSE))
  # multi
  expect_equal(qqplot.multi,qqplot_rubin_abpool(abpool.out.multi, plot.it = FALSE))
  expect_equal(qqplot.multi,qqplot_rubin_abpool(abpool.out.multi, plot.it = TRUE, add_qqline = FALSE))
  # one
  expect_equal(qqplot.one,qqplot_rubin_abpool(abpool.out.multi, parm = "X", plot.it = FALSE))
  expect_equal(qqplot.one,qqplot_rubin_abpool(abpool.out.multi, parm = "X", plot.it = TRUE, add_qqline = FALSE))
})
