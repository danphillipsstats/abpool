# `qqplot_rubin_abpool()` specification

## 1. Purpose
Generate a Q--Q plot comparing the quantiles given by Rubin's t-approximation to the samples generated from approximate Bayesian pooling (ABpool). To diagnose possible issues with Rubin's rules, by assessing the distribution against that of ABpool.
The function takes an object of class `abpool` and generates a Q--Q plot with the quantiles of Rubin's rules on the x-axis and the ABpool samples on the y-axis. Outputs the quantiles given by Rubin's t-approximation, and samples from ABpool.

## 2. Inputs
`object`
An object of class `abpool`, produced by the function `abpool()`.

`plot.it`
logical. Should the result be plotted? Default TRUE.

`qqline`
logical. Should a qqline be added? Default TRUE.

Possibly also:
`conf.level`
confidence level of the band. The default, NULL, does not lead to the computation of a confidence band

## 3. Output
results <- qqplot_rubin_abpool(object)
 - results$x
The quantiles given by Rubin's t-distribution. That is, the x coordinates of the points that were/would be plotted.
 - results$y
The samples from ABpool. That is, the y coordinates of the points that were/would be plotted.
 - results$p
The probabilities used to calculate the confidence interval. 
If conf.level was specified to qqplot, the list contains additional components lwr and upr defining the confidence band

## 4. Function overview
 - Extract the quantities needed to calculate Rubin's rules from `object`
 - Calculate the location, scale and degrees of freedom for Rubin's approximation.
 - Generate `k` quantiles from Rubin's t-distribution, where `k` is the number of abpool samples, using ppoints.
 - Plot qqplot(x=rubin_quantiles,y=object$samples,...)
 - Output x, y and p.

## 5. Errors
 - If the object is not of class `abpool`.

## 7. Warnings
Posibly if J > 1?

## 8. Randomness
   The function does not take a seed argument. Reproducibility is obtained using set.seed() in the usual R manner.

## 9. Minimal examples

