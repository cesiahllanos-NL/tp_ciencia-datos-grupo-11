# coma(): da formato a un número con coma decimal (estilo argentino).
# Ejemplo: coma(5.43, 1) devuelve "5,4"
coma <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
