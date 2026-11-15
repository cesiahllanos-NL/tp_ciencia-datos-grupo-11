# desvio_ponderado(): desvío estándar con ponderadores (clase 5).
desvio_ponderado <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  m <- weighted.mean(x, w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}
