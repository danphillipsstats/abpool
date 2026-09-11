INPUT VALIDATION

Common
[-] object type mira
[-] object type list
[-] object invalid (length 0, not a list, list of estimates and variances)
[-] parameters valid numeric (length 1, > 1)
[-] parameters valid character (length 1, > 1)
[-] parameters invalid numeric (NA, Inf, neg, larger than p)
[-] parameters invalid character (NA, Inf, not a parameter)
[-] parameters contains duplicates
[-] parameters invalid other (booleian, matrix)
[ ] df valid
[ ] df invalid
[ ] J valid
[ ] J invalid
[ ] m = 1

MODELS

Valid
[ ] lm (standard, with polynomial/spline terms)
[ ] glm
[ ] Cox
[ ] Other common classes (mixed models, etc)

Invalid
[ ] A model which vcov/coef doesn't work for
[ ] Imputations with infinite parameter/variance estimates
[ ] Unspecified dfcom, models df.residual won't extract from (Cox etc.)
[ ] Different models across elements (df.residual same) - I don't think this will error currently, but it maybe should
[ ] Different n (separately, p) and hence df.residual across elements (can be done by some imputations being NA, and complete case analysis automatically applied by model)
[ ] Different order of parameters across elements

SPECIFICATION (for both scalar and multivariate)
[ ] J = 1
[ ] J > 1
[ ] df = 19
[ ] df = Inf
[ ] parameters = NULL
[ ] parameters length > 1
[ ] parameters length 1
[ ] parameters reordered

SAMPLING
[ ] abpool(mira_object,) = abpool(mira_object$analyses)

OUTPUT
[ ] samples (scalar/multivariate) - length, type, dimension, names
[ ] estimates (scalar/multivariate) - length, type, dimension, names
[ ] variance (scalar/multivariate) - length, type, dimension, names
[ ] J matches input
[ ] dfcom matches input
[ ] dfcom extracted
[ ] parameters matches input
[ ] parameters = NULL actually selects all parameters (validate with names from samples, estimates, variances)
[ ] parameters reordered does it for estimates and variances correctly
