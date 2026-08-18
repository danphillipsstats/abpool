#' abpool_sample
#'
#' Sample via approximate Bayesian pooling (ABpool).
#'
#' For each element in estimates and variances, from a t-distribution with mean given by estimates, and scale given by variances.
#'
#' @param estimates Estimates from the imputed datasets.
#' @param variances Associated variances or variance-covariance matrices.
#' @param df Complete-data degrees of freedom.
#' @param J Number of samples per imputation.
#'
#' @return Posterior samples from approximate Bayesian pooling.
#'
#' @references Phillips, Christodoulou and Steinsaltz (XXXX)
#'
#' @examples
#' estimates <- c(...)
#' covariances <- c(...)
#' abpool_sample(estimates, covariances, df = Inf)
#'
#' estimates <- list(
#' c(x = ..., z = ...),
#' c(x = ..., z = ...)
#' )
#' covariances <- list(
#'   matrix(..., nrow = 2, ncol=2),
#'   matrix(..., nrow = 2, ncol=2)
#' )
#' abpool_sample(estimates, covariances, df = 19)
#'
#'
#' @export
abpool_sample <- function(estimates, variances, df, J = 1) {

  # 1. Validate inputs
  # df
  if (length(df) != 1L || !is.numeric(df) || is.na(df) || df <= 0 ){
    stop("`df` must be a single positive value.")
  }
  # J
  if (length(J) != 1L || !is.numeric(J) || J <= 0 || J != round(J) || !is.finite(J) ){
    stop("`J` must be a single positive integer.")
  }

  # 2. Determine whether this is the scalar or multivariate case
  if (is.numeric(estimates) && is.numeric(variances)){
    # Scalar case
    #####
    # Define m
    m <- length(estimates)

    # Validate inputs
    if (m==0){stop("The number of imputations `m` (equal to the length of the estimates and variances vectors) must be positive.")}
    if (length(variances)!=m){stop("The length of `estimates` and `variances` must be equal.")}
    if (!is.null(dim(estimates)) || !is.null(dim(variances))){stop("`estimates` and `variances` must be numeric vectors.")}
    if (any(!is.finite(estimates))){stop("`estimates` must only contain finite values.")}
    if (any(!is.finite(variances))){stop("`variances` must only contain finite values.")}
    if (any(variances<0)){stop("`variances` must only contain non-negative values.")}

    ######
    # Run function
    if (is.infinite(df)){samples <- rnorm(n=m,mean=estimates,sd=sqrt(variances))} # Gaussian/normal case
    else {samples <- rt(n=m,df)*sqrt(variances)+estimates} # t case
    # Add J > 1 later
  }
  else if (is.list(estimates) && is.list(variances)){
    # Multivariate case

  }
  else{
    stop("`estimates` and `variances` must either both be numeric vectors ",
         "(scalar case) or both be lists (multiple-parameter case).")
  }
  # 3. Scalar case

  # 4. Multivariate case

  # 5. Return samples
  return(samples)
}
