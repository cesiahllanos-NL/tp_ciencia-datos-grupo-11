# pct(): convierte una proporción en texto con porcentaje y coma decimal.
# Ejemplo: pct(0.0876) devuelve "8,8%". Depende de coma().
source("utils/coma.R")
pct <- function(x) paste0(coma(100 * x), "%")
