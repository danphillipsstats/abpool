#' Approximate Bayesian pooling
#'
#' Perform approximate Bayesian pooling (ABpool).
#'
#' Given model fits from multiply imputed datasets, `abpool()` generates posterior samples using approximate Bayesian pooling (ABpool).
#' The function first extracts estimates and variances for the parameters from each fitted model. It then generates ABpool posterior samples from a t-distribution with location given by the estimates, and squared scale (or scale matrix) given by the variances. For `dfcom = Inf`, draws are from the corresponding Gaussian distribution.
#'
#' @param object A list of model fits, where the `l`th element gives the model fit to the `l`th imputed dataset. Can be a `mira` object created by `mice::with()`
#' @param parameters A character vector of parameter names, or a numeric vector of parameter positions to include in ABpool. If `parameters = NULL`, ABpool will be performed for all parameters in the model. Parameter samples are returned in the order specified.
#' @param dfcom The complete-data degrees of freedom used for the t-distributed complete-data posterior approximation. `dfcom = Inf` corresponds to a Gaussian approximation. If `NULL`, `abpool()` attempts to extract the degrees of freedom from each fitted model using `df.residual()`; an error is returned if extraction fails or the extracted values differ across imputations. We recommend specifying `dfcom` when the appropriate complete-data degrees of freedom are known.
#' @param J Number of samples drawn per imputed dataset. Defaults to `1`.
#'
#' @details
#' The `abpool` function samples from an approximation to the observed-data posterior distribution, assuming the completed-data posterior distribution given each imputed dataset is t-distributed, with degrees of freedom equal to `dfcom`, location given by the estimate, and squared scale (or scale matrix) given by the variance estimate.
#' The input `object` may be:
#' 1. A list of model fit objects, with each element generated from a function such as `lm()`, `glm()`, `coxph()` etc.
#' 2. An object of class `mira` generated from `with()`, on a `mids` object from the `mice` package. (Missing data can be imputed by `pool()` which generates a `mids` object. `with()` performs repeated analyses on the imputed data and generates a `mira` object.)
#'
#' Inference using ABpool generally requires substantially more imputations than Rubin's rules. We recommend using at least 200 imputations when comparing Rubin's rules with ABpool using a Q--Q plot (`QQ_compare_abpool_rubin()`), and 1000 or more imputations may be required for final inference using ABpool to achieve appropriate coverage (Phillips, Christodoulou and Steinsaltz, XXXX).
#'
#' @return An object of class `abpool`. The object is a list containing
#' \describe{
#'   \item{samples}{ABpool posterior samples. A numeric vector of length `m x J` for a single parameter, or an `m * J` by `p` matrix for `p` parameters.}
#'   \item{estimates}{Parameter estimates extracted from the fitted models. A numeric vector of length `m` for a single parameter, or a list of `m` numeric vectors of length `p` for multiple parameters.}
#'   \item{variances}{Variance estimates extracted from the fitted models. A numeric vector of length `m` for a single parameter, or a list of `m` covariance matrices of dimension `p` by `p` for multiple parameters.}
#'   \item{dfcom}{The complete-data degrees of freedom used for sampling.}
#'   \item{parameters}{A character vector containing the names of the parameters included in ABpool, in the order used for sampling.}
#'   \item{J}{The number of samples drawn per imputed dataset.}
#'   \item{m}{The number of imputed datasets.}
#'   \item{imputation}{An integer vector of length `m * J` identifying the imputed dataset associated with each element (scalar case) or row (multivariate case) of `samples`.}
#' }
#'
#' @references Phillips, Christodoulou and Steinsaltz (XXXX)
#'
#' @seealso [QQ_compare_abpool_rubin()] to compare Rubin's rules and ABpool in a quantile-quantile (Q--Q) plot, [abpool_sample()] to sample from a vector/list of estimates and variances, [mice::with.mids()] to generate a `mira` object containing a list of model fits, [mice::mice()] to impute missing data, the output of which can be used as an input for [with()] to generate the `mira` object.
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
      parameters = parameters,
      imputation = rep(seq_len(m), each = J),
      J = J,
      m = m
    ),
    class = "abpool"
  )
}
