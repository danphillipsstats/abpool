#' Credible intervals for approximate Bayesian pooling
#'
#' Calculate credible intervals using posterior samples from approximate Bayesian pooling (ABpool).
#'
#' Credible intervals are calculated using empirical quantiles of the posterior samples returned by [abpool()].
#'
#' @param object An object of class `abpool`, returned by [abpool()].
#' @param parm A character or numeric vector specifying the parameters for which credible intervals should be calculated. If `NULL`, intervals are calculated for all parameters.
#' @param level Credible interval level. A single numeric value between 0 and 1. Defaults to `0.95`.
#' @param ... Additional arguments. Currently unused.
#'
#' @return A matrix containing the lower and upper credible limits for each requested parameter, with one row per parameter.
#'
#' @seealso [abpool()]
#'
#' @examples
#'
#' # Generate data
#' set.seed(1)
#' n <- 400; nobs <- 40
#' X <- rnorm(n); Z <- sqrt(0.9)*rnorm(n) + sqrt(0.1)*X
#' beta_0 <- 0; beta_X <- 0.8; beta_Z <- 0.2
#' eta <- beta_0 + X*beta_X + Z*beta_Z
#' Y <- rbinom(n, size = 1, p = exp(eta)/(1 + exp(eta)) )
#' X[1:(n-nobs)] <- NA
#' log.data <- data.frame(Y = Y, X = X, Z = Z)
#' m <- 200
#' # Impute using mice (200 imputations, predictive mean matching)
#' imp <- mice::mice(log.data, m = m, method = "pmm", print = FALSE)
#' fits <- with(
#'   imp,
#'   glm(Y ~ X + Z, family = binomial)
#' )
#' # Fit ABpool
#' abpool.out <- abpool(fits)
#' confint(abpool.out)
#'
#' @export
confint.abpool <- function(object, parm = NULL, level = 0.95, ...) {
  # Validation
  # level
  if (length(level) != 1L || !is.numeric(level) || level <= 0 || level >= 1) {
    stop("`level` must be a single numeric value between 0 and 1.")
  }
  # parm - change to character vector
  if (is.null(parm)){parm <- object$parameters}
  if (is.numeric(parm)){
    valid_numeric <- all(parm==round(parm)) && all(is.finite(parm)) && all(abs(parm)<=length(object$parameters)) && (all(parm>0) || all(parm<0))
    if (!valid_numeric){stop("Numeric `parm` must contain non-zero integer indices, either all positive or all negative, whose absolute values are no greater than the number of parameters. Alternatively `parm` may be `NULL`, in which case all parameters will be included, or a character vector of parameter names.")}
    parm <- object$parameters[parm]
  }
  if (!is.character(parm)){stop("`parm` must be a numeric or character vector of parameter indices or names.")}
  if (length(parm)==0){stop("No parameters were selected by `parm`.")}
  if (!all(parm %in% object$parameters)){stop("For `parm` a character vector of parameter names, every element of `parm` must also be an element of `object$parameters`, the parameters in the ABpool sample. Otherwise `parm` may be `NULL`, in which case all parameters will be used, or a numeric vector giving indices of `object$parameters` to output.")}
  # Function
  samples <- object$samples
  alpha <- (1 - level)/2
  p_alphas <- c(alpha, 1 - alpha)
  pct <- paste(format(100*p_alphas,trim=TRUE,scientific=FALSE,digits=3),"%")
  if (is.matrix(samples)){
    ci <- t(apply(samples[,parm, drop = FALSE],2,function(x) quantile(x,p_alphas)))
    dimnames(ci) <- list(parm,pct)
  } else {
    ci <- matrix(quantile(samples,p_alphas), 1,2,dimnames = list(parm,pct))
  }
  ci
}
