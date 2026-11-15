# fmt_p(): da formato a un p-valor: "p < 0,001" o "p = 0,022".
# Depende de coma(), por eso la carga acá.
source("utils/coma.R")
fmt_p <- function(p) ifelse(p < 0.001, "p < 0,001", paste0("p = ", coma(p, 3)))
