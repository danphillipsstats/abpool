#' Credible intervals for approximate Bayesian pooling
#'
#' Calculate credible intervals from approximate Bayesian pooling (ABpool)
#' posterior samples.
#'
#' Credible intervals are calculated using empirical quantiles of the
#' posterior samples returned by [abpool()].
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
#' # To add once the abpool() examples are finalised
#'
#' @export
confint.abpool <- function(object, parm = NULL, level = 0.95, ...) {
  # Validation
  # level
  if (length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must be a single numeric value between 0 and 1.")
  }
  # parm - change to character vector
  if (is.null(parm)){parm <- object$parameters}
  if (is.numeric(parm)){
    valid_numeric <- all(parm==round(parm)) && all(parm > 0) && all(is.finite(parm)) && all(parm<=length(object$parameters))
    if (!valid_numeric){stop("For `parm` a numeric vector of parameter indices, the entries must be positive integers, of size less than or equal to the number of elements in `object$parameters`.")}
    parm <- object$parameters[parm]
  }
  if (!all(parm %in% object$parameters)){stop("For `parm` a character vector of parameter names, every element of `parm` must also be an element of `object$parameters`, the parameters in the ABpool sample. Otherwise `parm` may be `NULL`, in which case all parameters will be used, or a numeric vector giving indices of `object$parameters` to output.")}
  # Function
  samples <- object$samples
  alpha <- (1 - level)/2
  p_alphas <- c(alpha, 1 - alpha)
  pct <- paste(format(100*p_alphas,trim=TRUE,scientific=FALSE,digits=3),"%")
  if (is.matrix(samples)){
      ci <- t(apply(samples[,parm, drop = FALSE],2,function(x) quantile(x,p_alphas)))
      dimnames(ci) <- list(parm,pct)
  }
  else {
    ci <- matrix(quantile(samples,p_alphas), 1,2,dimnames = list(parm,pct))
  }
  ci
}
