VALIDATION

Inputs
[ ] object type abpool (accept/reject)
[ ] parm valid numeric (NULL, length 1, > 1, reordered, negative)
[ ] parm valid character (length 1, > 1, reordered)
[ ] parm invalid numeric (pos and neg, larger than p)
[ ] parm invalid character (not a parameter, a parameter in lm but not included in abpool)
[ ] plot.it valid (TRUE/FALSE)
[ ] add_qqline valid (TRUE/FALSE)
[ ] plot.it/add_qqline invalid (non-booleian)
[ ] other graphical parameters provided (xlab, ylab, main, pch. scalar and multivariate)

PLOTTING
[ ] plot.it (TRUE/FALSE) defines plots
[ ] add_qqline (TRUE/FALSE) defines line
[ ] parm defines number of plots
[ ] xlab, ylab, main - user specified doesn't change
[ ] pch impacts plot

OUTPUT
[ ] returned object is invisible
[ ] assignment works `out <- qqplot_rubin_abpool()`
[ ] parm defines list structure correctly
[ ] length of outputs x,y,p
[ ] x agrees with expected Rubin t-quantiles from hand-calculated scalar example (and mice?) (dfcom = Inf/finite)
[ ] y is sorted object$samples
[ ] p is ppoints(k)
[ ] plot.it and add_qqline don't affect returned values
[ ] Putting outputted x and y into qqplot gives same plot
