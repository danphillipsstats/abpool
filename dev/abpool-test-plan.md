INPUT VALIDATION

Common
[-] object type mira
[-] object type list
[-] object invalid (length 0, not a list, list of estimates and variances)
[-] parameters valid numeric (length 1, > 1, reordered)
[-] parameters valid character (length 1, > 1, reordered)
[-] parameters invalid numeric (NA, Inf, neg, larger than p)
[-] parameters invalid character (NA, Inf, not a parameter)
[-] parameters contains duplicates
[-] parameters invalid other (booleian, matrix)
[-] dfcom valid
[-] dfcom invalid
[-] dfcom extracted (success/fail)
[-] J valid
[-] J invalid
[-] m = 1

MODELS

Valid
[-] lm (standard, with polynomial/spline terms)
[-] glm
[-] Cox
[?] Other common classes (mixed models, etc) - possibly add testing in future

Invalid
[-] A model which vcov/coef doesn't work for
[-] Imputations with infinite parameter/variance estimates
[-] Unspecified dfcom, models df.residual won't extract from (Cox etc.)
[-] Different n (separately, p) and hence df.residual across elements (can be done by some imputations being NA, and complete case analysis automatically applied by model)
[-] Different order of parameters across elements

SAMPLING
[-] abpool(mira_object,) = abpool(mira_object$analyses) (tested just for lm())

OUTPUT
[-] samples (scalar/multivariate/J=1/J>1) - length/dimension, type, names
[-] estimates (scalar/multivariate) - length/dimension, type, names
[-] variance (scalar/multivariate) - length/dimension, type, names
[-] m matches input length
[-] J matches input
[-] dfcom output correct (matches input/extracted matches df.residual)
[-] parameters matches input
[-] parameters = NULL actually selects all parameters (validate with names from samples, estimates, variances)
[ ] parameters reordered does it for estimates and variances correctly
