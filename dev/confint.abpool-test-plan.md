VALIDATION

Inputs
[-] object type abpool (accept/reject)
[-] parm valid numeric (NULL, length 1, > 1, reordered, negative)
[-] parm valid character (length 1, > 1, reordered)
[-] parm invalid numeric (pos and neg, larger than p)
[-] parm invalid character (not a parameter, a parameter in lm but not included in abpool)
[-] level valid (unspecified, 0.5)
[-] level invalid (>1, <0, vector, character)

OUTPUT
[-] one parameter - length/dimension, type, names
[-] multi parameter - length/dimension, type, names
[-] one parameter - correctly applies quantile
[-] multi parameter - correctly applies quantile
[-] negative parm chooses correct parameters
[-] positive parm chooses correct parameters
