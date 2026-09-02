# Experimental parameter-name validation for abpool_sample()
#
# Not currently used in the package.
# Intended to detect inconsistent parameter ordering in the multivariate case between:
# - names(estimates[[l]])
# - rownames(variances[[l]])
# - colnames(variances[[l]])
#
# Before implementation:
# - replace sapply with lapply/vapply
# - check names across all imputations, not judt the first
# - handle mixtures of NULL/default/informative names
# - decide definitively whether mismatches produce warnings or errors
# - add unit tests
#
# Positional ordering remains the authoritative ordering in the current
# implementation.

# Add a test for element names not matching
estimates_names <- lapply(estimates,names)
# Define ignorable names
ignorable.estimates.names <- is.null(estimates_names[[1]]) || all(estimates_names[[1]]==c(1:p)) || all(estimates_names[[1]]==as.character(1:p))
variances_row_names <- lapply(variances,rownames)
variances_col_names <- lapply(variances,colnames)
ignorable.variances.row.names <- is.null(variances_row_names[[1]]) || all(variances_row_names[[1]]==c(1:p)) || all(variances_row_names[[1]]==as.character(1:p))
ignorable.variances.col.names <- is.null(variances_col_names[[1]]) || all(variances_col_names[[1]]==c(1:p)) || all(variances_col_names[[1]]==as.character(1:p))
ignorable.variances.names <- ignorable.variances.row.names && ignorable.variances.col.names
if (!ignorable.estimates.names){
  if (!all(apply(estimates_names,2,function(x){all(x==estimates_names[,1])}))){stop("Each element of `estimates` must have the same parameter names, in the same order.")}
}
if (!ignorable.variances.names){
  if (!all(apply(variances_row_names,2,function(x){all(x==variances_row_names[,1])})) ||
      !all(apply(variances_col_names,2,function(x){all(x==variances_col_names[,1])})) ||
      !all(sapply(1:p,function(j){all(variances_row_names[,j]==variances_col_names[,j])}))){stop("Each element of `variances` must have the same row and column names, in the same order.")}
}
if ((!ignorable.estimates.names) && (!ignorable.variances.names)){
  if(!all(sapply(1:p,function(j){all(estimates_names[,j]==variances_col_names[,j])}))){stop("The names of the elements of `estimates` do not match the column and row names of the elements of `variances`.",
                                                                                            "Please check the order is correct.")}
}
