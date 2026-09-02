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
    # 3. Scalar case
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
    if (J==1){
      if (is.infinite(df)){samples <- rnorm(n=m,mean=estimates,sd=sqrt(variances))} # Gaussian/normal case
      else {samples <- rt(n=m,df)*sqrt(variances)+estimates} # t case
    }
    else { # J > 1
      expanded_estimates <- rep(estimates,each=J)
      expanded_variances <- rep(variances,each=J)
      if (is.infinite(df)){samples <- rnorm(n=m*J,mean=expanded_estimates,sd=sqrt(expanded_variances))} # Gaussian/normal case
      else {samples <- rt(n=m*J,df)*sqrt(expanded_variances)+expanded_estimates} # t case
    }
  }
  else if (is.list(estimates) && is.list(variances)){
    # 4. Multivariate case
    #####
    # Define m
    m <- length(estimates)
    p <- length(estimates[[1]])

    # Validate inputs
    if (m==0){stop("The number of imputations `m` (equal to the length of the estimates and variances lists) must be positive.")}
    if (length(variances)!=m){stop("The length of the `estimates` and `variances` lists must be equal.")}
    if (!all(sapply(estimates,is.numeric)) || !all(sapply(lapply(estimates,dim),is.null)) || !all(sapply(estimates,length)==p)){
      stop("`estimates` must be a list of numeric vectors of the same length.")
    }
    if (!all(sapply(variances,is.numeric)) || !all(sapply(variances,is.matrix)) || !all(sapply(variances,nrow)==sapply(variances,ncol)) || !all(sapply(variances,nrow)==nrow(variances[[1]]))){
      stop("`variances` must be a list of numeric matrices of the same dimension.")
    }
    if (!all(sapply(variances,nrow)==sapply(estimates,length))){
      stop("The number of rows/columns in each element of `variances` must be equal to the length of each entry in `estimates`.")
    }
    if (!all(sapply(estimates, function(x) all(is.finite(x))))){stop("`estimates` must only contain finite values.")}
    if (!all(sapply(variances, function(x) all(is.finite(x))))){stop("`variances` must only contain finite values.")}

    tol <- sqrt(.Machine$double.eps)
    if (!all(sapply(variances,isSymmetric, tol = tol))){stop("Each element of `variances` must be a symmetric matrix.")}

    eigenvalues <- lapply(variances,function(x){eigen(x, symmetric = TRUE, only.values = TRUE)$values})
    if (!all(sapply(eigenvalues,function(x){all(x >= -tol * abs(x[1]))}))){stop("Each element of `variances` must be a positive semi-definite matrix.")}


    ######
    # Run function
    if (J==1){
      if (is.infinite(df)){
        # Gaussian/normal case
        samples <- t(vapply(seq_len(m),function(i) mvtnorm::rmvnorm(n=1,mean=estimates[[i]],sigma=variances[[i]]), numeric(p)))
        }
      else {
        # t case
        # Generate multivariate Gaussian
        samples <- t(vapply(seq_len(m),function(i) mvtnorm::rmvt(n=1,sigma=variances[[i]], df = df) + estimates[[i]], numeric(p)))
        }
    }
    else { # J > 1
      if (is.infinite(df)){
        # Gaussian/normal case
        samples <- t(vapply(rep(seq_len(m),each=J),function(i) mvtnorm::rmvnorm(n=1,mean=estimates[[i]],sigma=variances[[i]]), numeric(p)))
      }
      else {
        # t case
        samples <- t(vapply(rep(seq_len(m),each=J),function(i) mvtnorm::rmvt(n=1,sigma=variances[[i]], df = df) + estimates[[i]], numeric(p)))
      }
    }
    colnames(samples) <- names(estimates[[1]])
  }
  else{
    stop("`estimates` and `variances` must either both be numeric vectors ",
         "(scalar case) or both be lists (multiple-parameter case).")
  }

  # 5. Return samples
  return(samples)
}
