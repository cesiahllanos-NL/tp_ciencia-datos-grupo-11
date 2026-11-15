# indice_rand_ajustado(): índice de Rand ajustado entre dos particiones de las
# mismas personas, con ponderadores. Mide cuánto coinciden dos clasificaciones:
# 1 = son idénticas (aunque los grupos tengan otros números o nombres);
# 0 = coinciden tanto como lo haría una clasificación al azar.
#   a, b = vectores con el grupo de cada persona en cada partición
#   w    = ponderadores (opcional; si no se pasa, todas pesan igual)
indice_rand_ajustado <- function(a, b, w = NULL) {
  if (is.null(w)) w <- rep(1, length(a))
  datos <- data.frame(a = a, b = b, w = as.numeric(w))
  datos <- datos[!is.na(datos$a) & !is.na(datos$b) & !is.na(datos$w), ]

  tabla <- xtabs(w ~ a + b, data = datos)       # tabla de contingencia ponderada
  pares <- function(x) x * (x - 1) / 2          # cantidad de pares dentro de un grupo

  suma_celdas <- sum(pares(tabla))
  suma_filas  <- sum(pares(rowSums(tabla)))
  suma_cols   <- sum(pares(colSums(tabla)))
  total       <- pares(sum(tabla))

  esperado <- suma_filas * suma_cols / total
  maximo   <- (suma_filas + suma_cols) / 2
  (suma_celdas - esperado) / (maximo - esperado)
}
