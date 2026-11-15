# mediana_ponderada(): mediana con ponderadores (función de la clase 5).
# Ordena los valores y devuelve el primero cuya proporción acumulada de
# ponderadores llega a 0,5.
mediana_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}
