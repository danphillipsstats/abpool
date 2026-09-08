# `abpool()` specification

## 1. Purpose
The function takes a list of fitted models, or a mira object from mice::with, where the `l`th element contains the model fit on the `l`th imputed dataset.
The user may specify the relevant parameters, the complete-data degrees of freedom `dfcom`, and the number of samples per imputation `J`.
`abpool()` then extracts the estimates and variances from the list, and inputs these into the `abpool_sample()` function.
This samples from the approximate Bayesian pooling algorithm (ABpool) presented by Phillips, Christodoulou and Steinsaltz (2026).
Namely, for a user-inputted number of degrees of freedom `dfcom`, it samples from a t-distribution with dfcom degrees of freedom, centered at the estimate and with scale parameter determined by its associated variance estimate, for each imputation.

## 2. Mathematical definition
Let $\hat{\theta}^{*(l)}$ be a scalar- (or vector-) valued estimate calculated on the $l$th imputed dataset, with associated variance (or variance-covariance matrix) $U^{*(l)}$.
Let $\nu_{\text{com}}$ be the complete-data degrees of freedom, associated with inference on a complete dataset.
Let $m$ be the number of imputations.
For each $l$ in $1, \dots, m$, independently sample
$\theta^{*(l)}  \sim t_{\nu_{\text{com}}}(\hat{\theta}^{*(l)}, U^{*(l)})$
Equivalently for the scalar case,
$$
\theta^{*(l)}
=
\hat{\theta}^{*(l)}
+
\sqrt{U^{*(l)}}\psi^{(l)},
\qquad
\psi^{(l)} \sim t_{\nu_{\mathrm{com}}}(0,1).
$$
Equivalently for the multivariate case,
$$
\theta^{*(l)}
=
\hat{\theta}^{*(l)}
+
L^{(l)}Z^{(l)}/\sqrt{T^{(l)}/\nu_{\mathrm{com}}},
\qquad
Z^{(l)} \sim N_p(0,I_p), T^{(l)} \sim \Xi^2_{\nu_{\mathrm{com}}}
$$
independently, and
$$
L^{(l)}L^{(l)}^T = U^{*(l)}
$$

For $\nu_{\text{com}} = \infty$, the $t$-distribution is replaced by its Gaussian limit.
Here $U^{*(l)}$ is the scale parameter of the complete-data $t$-approximation. For finite $\nu_\text{com} > 2$, the variance of the resulting $t$-distribution is given by $U^{*(l)} \frac{\nu_\text{com}}{\nu_\text{com}-2}$.

## 3. Inputs
`object`
One of:
A list of model fits, where the `l`th entry in the list contains the model fit on the `l`th imputed dataset. Model fits are outputs from functions like `lm()`, `glm()`, `coxph()`, etc.
An object of class `mira`, produced by the function `mice::with()`. `mice::with()` fits the model to each imputed dataset.

`parameters`
A vector of parameter names, stating which parameters ABpool should draw samples from. If left NULL, all parameters in the model will be used.

`dfcom`
The number of complete-data degrees of freedom, $\nu_{\text{com}}$, which is then the degrees of freedom of the $t$-distribution from which ABpool samples.
Note `dfcom = Inf` is valid and corresponds to sampling from a Gaussian distribution.
If specified, the user-specified `dfcom` will be used instead of a value extracted from the fitted models.
If left NULL, `dfcom` will be extracted from `object`, or produce an error if this is not possible.
We recommend specifying `dfcom` where possible.

`J`
The number of samples to draw for each imputed dataset. `J` is an optional single positive integer-valued numeric, default J =  1 if not specified.

## 4. Output
results <- abpool(fits)
 - results$samples
For a scalar parameter, the function returns a vector containing the posterior draws.
For multiple parameters, it returns a matrix with one row per posterior draw and one column per parameter.
When J = 1, the $l$th row (or entry, for a scalar parameter) corresponds to a draw from the complete-data posterior approximation given the $lth$ imputation.
With J > 1, the total number of posterior draws is $mJ$. In this case the $J(l-1)+j$th row (or entry) corresponds to the jth draw from the complete-data posterior approximation from the lth imputation.
That is, returned draws are ordered:
rows 1 to J: imputation 1;
rows J+1 to 2J: imputation 2;
...
rows (m−1)J+1 to mJ: imputation m.
 - results$estimates
The estimates for each imputed dataset
 - results$variances
The variances for each imputed dataset
 - results$dfcom
The complete-data degrees of freedom
 - results$J
The number of samples per imputation

## 5. Function overview
 - Test inputs
 - if `mira` object, convert to list with `fits <- object$analyses`
 - Extract estimates for `parameters` using `coef(fits)`
 - Extract variance-covariance matrix for `parameters` using `vcov(fits)`
 - If dfcom = NULL, extract the complete-data degrees of freedom from the fitted models using the available model-specific method. An error is returned if this cannot be determined.
 - Run `abpool_sample(estimates,variances,dfcom,J)`
 - Output samples, esimates, variances, dfcom and possibly more.

## 6. Errors
 - Note that many errors will be handled by `abpool_sample()`.
 
The function should give an informative error if:
- The input is not a list
- There are no imputations.
- `estimates` or `variances` could not be extracted.
- `dfcom = NULL` and `dfcom` could not be extracted.
- Extracted dfcom gives different values for different entries in the list.

`abpool_sample()` should give an error if:
- Parameter dimensions are inconsistent across imputations.

## 7. Warnings
 - Possibly warn if `dfcom` is small?

Context on dfcom: The mean exists if dfcom > 1, the variance exists if dfcom > 2, skewness if dfcom > 3 and kurtosis if dfcom > 4. dfcom = Inf corresponds to generating from a Gaussian.
We will require dfcom > 0, and may give a warning if dfcom < 4.

## 8. Randomness
   The function does not take a seed argument. Reproducibility is obtained using set.seed() in the usual R manner.

## 9. Minimal examples

