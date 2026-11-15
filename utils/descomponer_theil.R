# descomponer_theil(): parte el Theil total en un componente ENTRE grupos y uno
# DENTRO de los grupos (función de la clase 12).
#   T_entre  = sum_g [ s_g * (mu_g / mu) * log(mu_g / mu) ]
#   T_dentro = sum_g [ s_g * (mu_g / mu) * T_g ]
# s_g = participación del grupo, mu_g = media del grupo, T_g = Theil del grupo.
# T_observado se calcula aparte: así verificar que entre + dentro = total
# prueba algo y no es una tautología.
# Requiere tidyverse y calcular_theil().
library(tidyverse)
source("utils/calcular_theil.R")

descomponer_theil <- function(x, w, grupo) {
  x <- as.numeric(x)
  w <- as.numeric(w)

  ok <- !is.na(x) & !is.na(w) & !is.na(grupo) & w > 0 & x > 0
  x <- x[ok]; w <- w[ok]; grupo <- grupo[ok]

  mu_total  <- weighted.mean(x, w)
  pob_total <- sum(w)

  detalle <- tibble(grupo = grupo, x = x, w = w) |>
    group_by(grupo) |>
    summarise(
      s_g     = sum(w) / pob_total,
      mu_g    = weighted.mean(x, w),
      theil_g = calcular_theil(x, w),
      .groups = "drop"
    ) |>
    mutate(
      aporte_entre  = s_g * (mu_g / mu_total) * log(mu_g / mu_total),
      aporte_dentro = s_g * (mu_g / mu_total) * theil_g
    )

  list(
    T_observado = calcular_theil(x, w),
    T_entre     = sum(detalle$aporte_entre),
    T_dentro    = sum(detalle$aporte_dentro),
    detalle     = detalle
  )
}
