# =====================================================================
# 00_main.R — Corre todo el análisis en orden.
# TP Ciencia de Datos · Grupo 11 · ENCoPraC 2022
#
# Antes de correr:
#   1. Abrir tp_g11.Rproj en RStudio (así la carpeta del proyecto queda
#      como directorio de trabajo y todas las rutas son relativas).
#   2. Tener instalados los paquetes que se listan abajo.
# =====================================================================

paquetes <- c("tidyverse", "scales", "ggrepel", "survey", "FactoMineR", "factoextra")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) {
  stop("Faltan paquetes: ", paste(faltantes, collapse = ", "),
       ". Instalalos desde la consola de R antes de correr este script.",
       call. = FALSE)
}

source("script/01_limpieza.R")            # raw/ -> input/base_limpia.rds
# 02_descriptivas.R                       # (pendiente) descriptivas antes y después
source("script/03_faltantes_outliers.R")  # diagnóstico de faltantes y outliers
source("script/04_chi_cuadrado.R")        # P1: chi², Rao-Scott y CMH
source("script/05_clustering.R")          # P2: ACM + clasificación de Ward
# 06_robustez_clusters.R                  # (pendiente) 4 y 6 clusters
source("script/07_gini_theil.R")          # tercer método: Gini, Lorenz y Theil
source("script/08_graficos_instancia3.R") # gráficos exploratorio y comunicativo (JPG)

message("Todo listo. Gráficos en output/graficos y tablas en output/tablas.")
