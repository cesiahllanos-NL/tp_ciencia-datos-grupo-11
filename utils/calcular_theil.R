# calcular_theil(): índice de Theil ponderado (función de la clase 12).
#   T = sum_i [ w_i * (x_i / mu) * ln(x_i / mu) ]   con sum w_i = 1
# Solo usa valores estrictamente positivos (el logaritmo no admite ceros).
calcular_theil <- function(x, w = NULL) {
  x <- as.numeric(x)
  if (is.null(w)) w <- rep(1, length(x))
  w <- as.numeric(w)

  ok <- !is.na(x) & !is.na(w) & w > 0 & x > 0
  x <- x[ok]; w <- w[ok]

  w_rel <- w / sum(w)
  media <- sum(x * w_rel)
  ratio <- x / media

  sum(w_rel * ratio * log(ratio))
}
