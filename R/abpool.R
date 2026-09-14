#' Approximate Bayesian pooling
#'
#' Perform approximate Bayesian pooling (ABpool).
#'
#' Given a list of model fits to each imputed dataset, pool the estimates via approximate Bayesian pooling (ABpool).
#' The function first extracts estimates and variances for the parameters from each fitted model. It then generates ABpool posterior samples from a t-distribution with location given by the estimates, and squared scale given by the variances. For `dfcom = Inf`, draws are from the corresponding Gaussian distribution.
#'
#' @param object A list of model fits, where the `l`th element gives the model fit to the `l`th imputed dataset. Can be a `mira` object created by `mice::with()`
#' @param parameters A character vector of parameter names, or a numeric vector of parameter positions for which to perform ABpool. If `parameters = NULL`, ABpool will be performed for all parameters in the model. Where relevant, the order of parameters supplied by the user will be retained.
#' @param dfcom Complete-data degrees of freedom; the degrees of freedom for the t-distributed completed-data posterior approximation. We recommend the user to specify `dfcom` where appropriate. If `dfcom = Inf`, samples are drawn from the equivalent Gaussian distribution. If `dfcom = NULL`, the estimated degrees of freedom will be extracted from the fitted models, where possible.
#' @param J Number of samples per imputation. Default `J = 1` sample per imputed dataset.
#'
#' @details
#' The `abpool` function samples from an approximation to the observed-data posterior distribution, assuming the completed-data posterior distribution given each imputed dataset is t-distributed, with degrees of freedom equal to `dfcom`, location given by the estimate, and squared scale given by the variance estimate.
#' The input `object` may be:
#' 1. A list of model fit objects, with each element generated from a function such as `lm()`, `glm()`, `coxph()` etc.
#' 2. An object of class `mira` generated from `mice::with()`, from the `mice` package.
#'
#' Note the required number of imputations to fit ABpool is much larger than for Rubin's rules. We recommend 1000 or more imputations may be required for inference via ABpool to give appropriate coverage (Phillips, Christodoulou and Steinsaltz, XXXX). We recommend 200 or more imputations to compare the distribution of Rubin's rules to ABpool samples in a Q--Q plot. This can be done via the function `QQ_compare_abpool_rubin()`.
#'
#' @return An object of class `abpool`. The object is a list containing
#' \describe{
#'   \item{samples}{Samples from approximate Bayesian pooling. Samples from approximate Bayesian pooling. These will either be a vector of length `m x J` for a single parameter, or an `m x J` by `p` matrix for multiple parameters. Samples `1:J` are drawn from the first completed-data posterior approximation, etc. with samples `(l-1)+(1:J)` being from the posterior approximation for the `l`th imputation, for `l` in `1` to `m`.}
#'   \item{estimates}{Estimates for each imputed dataset. Either a vector of length `m` (scalar case), or a list of length `m`, with each entry a vector of length `p` (multivariate case).}
#'   \item{variances}{Associated variances or variance-covariance matrices for each imputed dataset. Either a vector of length `m` (scalar case), or a list of length `m`, with each entry a `p` by `p` matrix (multivariate case).}
#'   \item{dfcom}{The complete-data degrees of freedom used for sampling.}
#'   \item{J}{The number of samples drawn per imputed dataset.}
#'   \item{m}{The number of imputed datasets.}
#'   \item{parameters}{The names of the parameters for which the ABpool samples were drawn, in the order used for sampling.}
#' }
#'
#' @references Phillips, Christodoulou and Steinsaltz (XXXX)
#'
#' @seealso [abpool_sample()] to sample from a vector/list of estimates and variances, [with()] to generate a `mira` object containing a list of model fits, [mice::mice()] to impute missing data, the output of which can be used as an input for [with()] to generate the `mira` object.
#'
#' @examples
#'
#'
#'
#' @export
abpool <- function(object, parameters = NULL, dfcom = NULL, J = 1) {
  #####
  # 1. Validate inputs
  if (!is.null(parameters)){
    if (anyDuplicated(parameters)){stop("`parameters` must not contain duplicate entries.")}
    valid_vector <- is.null(dim(parameters))
    valid_type <- is.character(parameters) ||
      (is.numeric(parameters) && all(parameters==round(parameters)) && all(parameters > 0) && all(is.finite(parameters)))
    valid_values <- length(parameters) >=1 && all(!is.na(parameters))
    if (!valid_vector || !valid_type || !valid_values){
      stop("`parameters` must be a character vector of parameter names, a numeric vector of parameter orders, or NULL, in which case all parameters will be used.")
    }
  }

  # 2. Extract estimates, variances and degrees of freedom
  # Extract list of model fits
  if (mice::is.mira(object)){
    fits <- object$analyses
  } else if (is.list(object)) {
      fits <- object
  } else {
        stop("object must be a list of model fits, where the `l`th element gives the model fit to the `l`th imputed dataset, or a `mira` object created by `mice::with()`")
      }
  if (length(fits) == 0L){stop("`object` must contain at least one fitted model.")}
  m <- length(fits)

  #####
  # i) Extract estimates and variances
  # Extract all estimates and variances
  estimates_all <- tryCatch(lapply(fits,coef), error = function(e) NULL)
  if (is.null(estimates_all)){stop("The entries in the list `object` must be such that `coef()` can be applied to them, to extract the estimates.")}
  variances_all <- tryCatch(lapply(fits,vcov), error = function(e) NULL)
  if (is.null(variances_all)){stop("The entries in the list `object` must be such that `vcov()` can be applied to them, to extract the variance-covariance matrices.")}
  # Check parameter names are consistent across estimates and variances
  coefnames <- lapply(estimates_all, names)
  colnames <- lapply(variances_all, colnames)
  rownames <- lapply(variances_all, rownames)
  if (!all(sapply(coefnames,function(x) identical(x,coefnames[[1]])))){stop("The names of the coefficients extracted from the entries of `object` by `coef` are inconsistent across imputations.")}
  if (!all(sapply(colnames,function(x) identical(x,colnames[[1]]))) || !all(sapply(rownames,function(x) identical(x,rownames[[1]])))){stop("The names of the coefficients extracted from the entries of `object` by `vcov` are inconsistent across imputations.")}
  if (!identical(rownames[[1]],colnames[[1]])){stop("The rownames and colnames of the variance matrix extracted from `object` by `vcov()` do not match.")}
  if (!(all(coefnames[[1]]==colnames[[1]]) && all(coefnames[[1]]==rownames[[1]]))){stop("The names of the coefficients extracted from the entries of `object` by `coef` are inconsistent with those extracted by `vcov`.")}
  # Check `parameters` are parameter entries (if numeric)
  if (is.numeric(parameters) && !all(parameters %in% seq_along(estimates_all[[1]]))){stop("`parameters` must be a character vector of parameter names, a numeric vector of parameter orders, or NULL, in which case all parameters will be used.",
                                                                               "Where a character vector is supplied, the entries must correspond to the names of the parameters extracted from the model fits by `coef()`")}
  # Check `parameters` are parameter names (if character)
  if (is.character(parameters) && !all(parameters %in% coefnames[[1]])){stop("`parameters` must be a character vector of parameter names, a numeric vector of parameter orders, or NULL, in which case all parameters will be used.",
                                                                               "Where a character vector is supplied, the entries must correspond to the names of the parameters extracted from the model fits by `coef()`")}
  # Choose all parameters (if null)
  if (is.null(parameters)){parameters <- seq_along(estimates_all[[1]])}

  #####
  # ii) Get relevant estimates and variances
  if (length(parameters) > 1){ # Multivariate case
    estimates <- lapply(estimates_all, function(x) x[parameters])
    variances <- lapply(variances_all, function(x) x[parameters,parameters,drop = FALSE])
  } else if (length(parameters) == 1){ # Scalar case
    estimates <- sapply(estimates_all, function(x) x[parameters])
    names(estimates) <- NULL
    variances <- sapply(variances_all, function(x) x[parameters,parameters,drop = TRUE])
  }
  # iii) Get dfcom-
  if (is.null(dfcom)){
    dfcom.vec <- tryCatch(vapply(fits, df.residual, numeric(1)), error = function(e) NULL)
  if (is.null(dfcom.vec)){stop("Please supply a value of `dfcom`. Usually, an appropriate choice will be one of: Inf, for a Gaussian complete-data posterior approximation; n - p, given sample size n and number of parameters p; or n - n_event, where n_event is the number of events in the Cox model.",
                               "If `dfcom` is not supplied, the entries in the list `object` must be such that `df.residual()` can be applied to them, to extract `dfcom`.")}
    if (!all(dfcom.vec==dfcom.vec[1])){stop("Extracted values of `dfcom` via `df.residual()` vary between imputations.")} # This error message could be improved
    # Perhaps update to extract df when not available as n - p and account for Cox model as n - nevent -- see mice::get.dfcom
    dfcom <- dfcom.vec[1]
  }

  #####
  # 3. Sample ABpool
  samples <- abpool_sample(estimates, variances, dfcom, J)

  # Turn parameters into character name
  if(is.numeric(parameters)){
    parameters <- coefnames[[1]][parameters]
  }

  # 4. Output
  structure(
    list(
      samples = samples,
      estimates = estimates,
      variances = variances,
      dfcom = dfcom,
      J = J,
      m = m,
      parameters = parameters
    ),
    class = "abpool"
  )
}
