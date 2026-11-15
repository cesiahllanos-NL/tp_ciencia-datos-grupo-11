# calcular_gini(): coeficiente de Gini ponderado, como el área entre la curva
# de Lorenz y la diagonal (función de la clase 12 del curso).
#   x = magnitud (acá, tragos); w = ponderadores (opcional).
# Procedimiento: ordenar de menor a mayor, acumular la proporción de unidades y
# de la magnitud, e integrar por trapecios.
calcular_gini <- function(x, w = NULL) {
  # as.numeric evita que integer * integer desborde y devuelva NA
  x <- as.numeric(x)
  if (is.null(w)) w <- rep(1, length(x))
  w <- as.numeric(w)

  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]

  orden <- order(x)
  x <- x[orden]; w <- w[orden]

  prop_unidades <- cumsum(w) / sum(w)
  prop_magnitud <- cumsum(x * w) / sum(x * w)

  area <- sum(diff(c(0, prop_unidades)) *
                (c(0, head(prop_magnitud, -1)) + prop_magnitud) / 2)

  1 - 2 * area
}
