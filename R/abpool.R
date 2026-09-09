#' Approximate Bayesian pooling
#'
#' Perform approximate Bayesian pooling (ABpool).
#'
#' Given a list of model fits to each imputed dataset, pool the estimates via approximate Bayesian pooling (ABpool).
#' The function first extracts estimates and variances for the parameters from each fitted model. It then generates ABpool posterior samples from a t-distribution with location given by the estimates, and squared scale given by the variances. For `dfcom = Inf`, draws are from the corresponding Gaussian distribution.
#'
#' @param object A list of model fits, where the `l`th element gives the model fit to the `l`th imputed dataset. Can be a `mira` object created by `mice::with()`
#' @param parameters The names of the parameters for which to perform ABpool. If `parameters = NULL`, ABpool will be performed for all parameters in the model.
#' @param dfcom Complete-data degrees of freedom; the degrees of freedom for the t-distributed completed-data posterior approximation. We recommend the user to specify `dfcom` where appropriate. If `dfcom = Inf`, samples are drawn from the equivalent Gaussian distribution. If `dfcom = NULL`, the estimated degrees of freedom will be extracted from the fitted models, where possible.
#' @param J Number of samples per imputation. Default `J = 1` sample per imputed dataset.
#'
#' @details
#' The `abpool` function samples from an approximation to the observed-data posterior distribution, assuming the completed-data posterior distribution given each imputation is t-distributed, with degrees of freedom equal to the corresponding complete-data degrees of freedom for the analysis, location given by the estimate, and squared scale given by the variance estimate.
#' The input `object` may be:
#' 1. A list of model fit objects, with each element generated from a function such as `lm()`, `glm()`, `coxph()` etc.
#' 2. An object of class `mira` generated from `mice::with()`, from the `mice` package.
#'
#' Note the required number of imputations to fit ABpool is much larger than for Rubin's rules. We recommend 1000 or more imputations may be required for inference via ABpool to give appropriate coverage (Phillips, Christodoulou and Steinsaltz, XXXX). We recommend 200 or more imputations to compare the distribution of Rubin's rules to ABpool samples in a Q--Q plot. This can be done via the function `QQ_compare_abpool_rubin()`.
#'
#' @return An object of class `abpool`. The object is a list containing
#' \describe{
#'   \item{samples}{Samples from approximate Bayesian pooling.}
#'   \item{estimates}{Estimates for each imputed dataset.}
#'   \item{variances}{Associated variances or variance-covariance matrices
#'   for each imputed dataset.}
#'   \item{dfcom}{The complete-data degrees of freedom used for sampling.}
#'   \item{J}{The number of samples drawn per imputed dataset.}
#' }
#'
#' @references Phillips, Christodoulou and Steinsaltz (XXXX)
#'
#' @seealso [abpool_sample()], [mice::with()]
#'
#' @examples
#'
#'
#'
#' @export
abpool_sample <- function(object, parameters = NULL, dfcom = NULL, J = 1) {

  # 1. Validate inputs
  if (!is.list(object)){stop("object must be a list of model fits, where the `l`th element gives the model fit to the `l`th imputed dataset, or a `mira` object created by `mice::with()`")}
  # OK: NULL OR (character AND !NA AND
  if (!is.null(parameters)){
    if(is.numeric(parameters)){
      # Test for numeric parameters
    } else if(!is.character(parameters) && (length(parameters)>=1 || !all(!is.na(parameters)))){
      stop("`parameters` must be a character vector of parameter names, a numeric vector of parameter orders, or NULL, in which case all parameters will be used.")
    } else("`parameters` must be a character vector of parameter names, a numeric vector of parameter orders, or NULL, in which case all parameters will be used.")
  }

  # 2. Extract estimates, variances and degrees of freedom
  # Extract list of model fits
  if (mice::is.mira(object)){fits <- object$analyses} else {fits <- object}
  if (length(fits) == 0L){stop("`object` must contain at least one fitted model.")}
  # i) Validate estimates and variances

  # ii) Get estimates and variances
  estimates <- lapply(fits,coef)
  variances <- lapply(fits,vcov)
  # iii) Get dfcom
  if (is.null(dfcom)){
    dfcom.vec <- sapply(fits,df.residual)
    if (!all(dfcom.vec==dfcom.vec[1])){stop("Extracted values of `dfcom` via `df.residual()` vary between imputations.")} # This error message could be improved
    # Perhaps update to extract df when not available as n - p and account for Cox model as n - nevent -- see mice::get.dfcom
    dfcom <- dfcom.vec[1]
  }


  # 3. Sample ABpool
  samples <- abpool_sample(estimates, variances, dfcom, J)
  # 4. Output
  structure(
    list(
      samples = samples,
      estimates = estimates,
      variances = variances,
      dfcom = dfcom,
      J = J
    ),
    class = "abpool"
  )
}
