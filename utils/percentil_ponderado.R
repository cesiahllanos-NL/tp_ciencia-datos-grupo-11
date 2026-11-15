# percentil_ponderado(): percentil p (entre 0 y 1) con ponderadores (clase 5).
percentil_ponderado <- function(x, w, p) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= p)[1]]
}
