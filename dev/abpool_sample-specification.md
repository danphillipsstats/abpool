# `abpool_sample()` specification

## 1. Purpose
The function takes a list or vector of inputted estimates and associated variances calculated on multiply imputed datasets.
It then samples from the approximate Bayesian pooling algorithm (ABpool) presented by Phillips, Christodoulou and Steinsaltz (2026).
Namely, for a user-inputted number of degrees of freedom (df), it samples from a t-distribution with df degrees of freedom, centered at the estimate and with scale parameter determined by its associated variance estimate, for each imputation. 

## 2. Mathematical definition
Let $\hat{\theta}^{*(l)}$ be the estimate calculated on the $l$th imputed dataset, with associated variance $U^{*(l)}$.
Let $\nu_{\text{com}}$ be the complete-data degrees of freedom, associated with inference on a complete dataset.
Let $m$ be the number of imputations.
For each $l$ in $1, \dots, m$, independently sample
$\theta^{*(l)}  \sim t_{\nu_{\text{com}}}(\hat{\theta}^{*(l)}, U^{*(l)})$
Equivalently,
$$
\theta^{*(l)}
=
\hat{\theta}^{*(l)}
+
\sqrt{U^{*(l)}}\psi^{(l)},
\qquad
\psi^{(l)} \sim t_{\nu_{\mathrm{com}}}(0,1).
$$
For $\nu_{\text{com}} = \infty$, the $t$-distribution is replaced by its Gaussian limit.
## 3. Inputs
`estimates`
For scalar-valued parameters, `estimates` is a numeric vector of length $m$, with $l$th entry `estimates[l]` containing the estimate $\hat{\theta}^{*(l)}$ for each imputed dataset $l = 1, \dots, m$.
For vector-valued parameters, `estimates` is a list of length $m$, with $l$th entry `estimates[[l]]` containing the estimate vector $\hat{\theta}^{*(l)}$ for each imputed dataset $l = 1, \dots, m$, with `length(estimates[[l]]) = p`, for $p$ the number of parameters.

`variances`
For scalar-valued parameters, `variances` is a vector of length $m$, with $l$th entry `variances[l]` containing the variance $U^{*(l)}$ for each imputed dataset $l = 1, \dots, m$.
For vector-valued parameters, `variances` is a list of length $m$, with $l$th entry `variances[[l]]` containing the covariance matrix $U^{*(l)}$ for each imputed dataset $l = 1, \dots, m$, with `dim(variances[[l]]) = c(p,p)`, for $p$ the number of parameters.

`df`
The number of complete-data degrees of freedom, $\nu_{\text{com}}$, which is then the degrees of freedom of the $t$-distribution from which ABpool samples.
Note `df = Inf` corresponds to the Gaussian case, where we sample from a Gaussian of mean `estimates` and variance `variances`.

`J`
The number of samples to draw for each imputed dataset. Optional input, assumed to be 1 if not specified.

## 4. Input conventions
 - List element l corresponds to imputation l
 - Within each parameter vector, parameter order defines the order in the covariance matrix. The kth entry of `estimates[[l]]` should correspond to the kth row/column of `variances[[l]]`
 - Covariance matrices are symmetric positive semi-definite p x p matrices
 - Parameter names are optional. Where given, the function will output a warning if the order does not match between `estimates` and `variances`, and across imputations. We will not reorder if the order does not match, but give a warning instead.
 - Automatic-looking names like "1","2","3",... or 1,2,3,... will be ignored in name consistency checks.

## 5. Output
For a scalar parameter, the function returns a vector containing the posterior draws. 
For multiple parameters, it returns a matrix with one row per posterior draw and one column per parameter. 
When J = 1, the $l$th row (or entry, for a scalar parameter) corresponds to a draw from the complete-data posterior approximation given the $lth$ imputation. 
With J > 1, the total number of posterior draws is $mJ$. In this case the $m(l-1)+j$th row (or entry) corresponds to the jth draw from the complete-data posterior approximation from the lth imputation. 
That is, returned draws are ordered:
rows 1 to J: imputation 1;
rows J+1 to 2J: imputation 2;
...
rows (m−1)J+1 to mJ: imputation m.


## 6. Errors
 - If the length of estimates and variances do not match length(estimates) != length(variances)
 - If m <= 0.
 - Scalar case: estimates must be numeric, only contain finite values (not NA, NaN or Inf)
 - Vector case: estimates musts be a list, each element must be numeric, have the same length, all entries finite
 - Scalar case: variances must be numeric, same length as estimates, all values finite, all values >= 0
 - Vector case: variances must be a list, same length as estimates, each element a matrix, each matrix p x p matching length of estimates entries, entries finite, symmetric positive semi-definite (within numerical tolerance).
 - df must be > 0 or df = Inf. Errors: df < 0, df == 0, df = NA, NaN, NULL, df = -Inf.
 - Error if: J < 1, J non integer, J = NA, J = NaN, J = Inf. Also require length(J) = 1.
 
The function should give an informative error if:

- `estimates` and `variances` contain different numbers of imputations.
- There are no imputations.
- `estimates` does not have the required structure.
- `variances` does not have the required structure.
- Parameter dimensions are inconsistent across imputations.
- Any estimate or variance/covariance entry is non-finite.
- Any scalar variance is negative.
- Any covariance matrix is not square or has incorrect dimensions.
- Any covariance matrix is not symmetric.
- Any covariance matrix is not positive semidefinite.
- `df` is missing, non-finite (other than `Inf`), or not strictly positive.
- `J` is missing, non-finite, non-integer, or less than 1.

## 7. Warnings
 - Warning if informative parameter names differ between the estimate vector and corresponding covariance matrix, or if informative names are inconsistent across imputations.
 - No warning for the automatic names we agreed to ignore.
 - Possibly something about df being too small so posterior moments are undefined
 - Variance 0?

The function should give a warning if:

- Informative parameter names are inconsistent between estimates and
  corresponding covariance matrices.
- Informative parameter names are inconsistent across imputations.

Automatic-looking names such as "1", "2", ... are ignored in these checks.

It is currently undecided whether to warn for small values of `df`. We expect to do this in abpool rather than abpool_sample.

## 8. Statistical requirements
   Mathematical/statistical conditions required of inputs.

## 9. Randomness
   How random-number generation and reproducibility work.

## 10. Computational requirements
    Vectorisation, memory, side effects, etc.

## 11. Minimal examples
    Scalar and multivariate examples.
