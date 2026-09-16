# `qqplot_rubin_abpool()` specification

## 1. Purpose
Generate a Q--Q plot comparing the quantiles given by Rubin's t-approximation to the samples generated from approximate Bayesian pooling (ABpool). To diagnose possible issues with Rubin's rules, by assessing the distribution against that of ABpool.
The function takes an object of class `abpool` and generates a Q--Q plot with the quantiles of Rubin's rules on the x-axis and the ABpool samples on the y-axis. Outputs the quantiles given by Rubin's t-approximation, and samples from ABpool.

## 2. Mathematical definition
The t-distribution given by Rubin's rules has location
`mean(estimates)`
squared scale
`mean(variances) + (1+1/m)*variance(estimates)`
and degrees of freedom defined by the following variables:
`lambda = (1+1/m)*b/(u+b)`
`if (dfcom = Inf){ `
`df = (m-1)/lambda^2`
`} else {`
` df_factor <- (1-lambda) * dfcom * (dfcom + 1)`
` df <- (m-1) * df_factor / ((m-1) * (dfcom + 3) + lambda^2 * df_factor)`
`}`

`ppoints()` will then be used to find the quantile probabilities, in a similar way to how qqplot/qqnorm does.


## 3. Inputs
`object`
An object of class `abpool`, produced by the function `abpool()`.

`parm`
The name of the parameter for which to generate the qqplot, required only when object contains multiple parameters. Default NULL.

`plot.it`
logical. Should the result be plotted? Default TRUE.

`add_qqline`
logical. Should a qqline be added? Default TRUE.

`...` Additional graphical arguments passed to `qqplot()`.

## 4. Output
Output returns invisibly.
If parm specifies a single parameter, the function returns a list containing `x`, `y`, and `p` (below). If multiple parameters are specified, it returns a named list, containing one such list for each parameter.
 - x
The quantiles of Rubin's \(t\)-approximation at the probabilities `results$p`. That is, the x coordinates of the points that were/would be plotted.
 - y
The ABpool posterior samples, sorted in increasing order. That is, the y coordinates of the points that were/would be plotted.
 - p
The probabilities used to calculate the quantiles of Rubin's t-distribution, given by ppoints(k). 

## 5. Function overview
For multiple parameters, loop over the parameters.
 - Extract the quantities needed to calculate Rubin's rules from `object`
 - Calculate the location, scale and degrees of freedom for Rubin's approximation.
 - Generate `k` quantiles from Rubin's t-distribution, where `k` is the number of abpool samples, using ppoints.
 - Plot qqplot(x=rubin_quantiles,y=object$samples,...)
 - Output x, y and p.

## 6. Errors
 - If the object is not of class `abpool`.
 No error if:
 - plot.it = FALSE, add_qqline = TRUE. 
 This is because qqplot_rubin_abpool(object, plot.it = FALSE) is equivalent to qqplot_rubin_abpool(object, plot.it = FALSE, add_qqline = TRUE) since TRUE is the default value. So this should simply not plot, without an error or a warning.

## 8. Warnings
Possibly mention in details issues with J > 1, but don't warn.

## 9. Randomness
   The function does not take a seed argument. Reproducibility is obtained using set.seed() in the usual R manner.

## 10. Minimal examples

