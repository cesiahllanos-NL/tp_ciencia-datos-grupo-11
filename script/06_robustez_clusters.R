# =====================================================================
# 06_robustez_clusters.R
# Robustez de los clusters del script 05: ¿qué pasa si se cambia la
# cantidad de grupos (4, 5 y 6) o las variables que los definen?
#
# Responde a la devolución de la instancia 2: "un análisis de robustez para
# los clusters, a ver qué pasa con menos clusters o cambiando las variables".
#
# Qué lee:  input/base_con_clusters.rds (lo genera 05_clustering.R).
# Qué guarda: output/tablas/t20 y t21, output/graficos/g18.
# Todo se pondera con WPER. Mismo procedimiento que el script 05: ACM sobre
# los perfiles de respuesta + clasificación jerárquica de Ward (HCPC).
# =====================================================================

library(tidyverse)
library(FactoMineR)
library(scales)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/estilo_graficos.R")   # fuente, tema y colores comunes
source("utils/coma.R")
source("utils/pct.R")
source("utils/indice_rand_ajustado.R")

dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

datos <- readRDS("input/base_con_clusters.rds")


activas_todas          <- c("alcohol", "intensidad", "tabaco", "marihuana", "tranquilizantes")
activas_sin_intensidad <- setdiff(activas_todas, "intensidad")

# Variante que resuelve un solapamiento de las variables activas: "alcohol en 12
# meses" (sí/no) y "intensidad" (basada en 30 días) comparten información, y la
# categoría "no tomó en 30 días" mezcla a quienes tomaron en el año con quienes no
# tomaron nunca. Acá las dos se unen en una sola variable de 5 categorías.
datos <- datos %>%
  mutate(alcohol_unificado = factor(
    case_when(
      alcohol == "Alcohol 12m: no" ~ "No tomó en 12 meses",
      P1M_AL == 0                  ~ "Tomó en 12 meses, no en 30 días",
      TRUE                         ~ as.character(intensidad)
    ),
    levels = c("No tomó en 12 meses", "Tomó en 12 meses, no en 30 días",
               "Intensidad: Menos de 3 tragos", "Intensidad: 3 a 4,9 tragos",
               "Intensidad: 5 o más tragos")))
activas_unificado <- c("alcohol_unificado", "tabaco", "marihuana", "tranquilizantes")

# Probar la función del índice de Rand con casos conocidos: una partición
# contra sí misma (o contra una copia con otros nombres) da 1.
grupos_prueba <- rep(c("a", "b", "c"), times = c(40, 35, 25))
stopifnot(abs(indice_rand_ajustado(grupos_prueba, grupos_prueba) - 1) < 1e-12)
stopifnot(abs(indice_rand_ajustado(grupos_prueba,
                                   recode(grupos_prueba, a = "x", b = "y", c = "z")) - 1) < 1e-12)


# ---- 1. Función que arma los clusters para unas variables y un k ---------
armar_clusters <- function(datos, activas, k) {
  perfiles <- datos %>%
    group_by(across(all_of(activas))) %>%
    summarise(peso = sum(WPER), .groups = "drop") %>%
    as.data.frame()

  # Cantidad de dimensiones posibles = categorías totales - cantidad de variables
  n_dim <- sum(map_int(droplevels(perfiles[, activas]), nlevels)) - length(activas)

  res_acm <- MCA(droplevels(perfiles[, activas]), row.w = perfiles$peso,
                 ncp = min(5, n_dim), graph = FALSE)
  set.seed(2026)
  res_hcpc <- HCPC(res_acm, nb.clust = k, consol = FALSE, graph = FALSE)

  perfiles$grupo <- res_hcpc$data.clust[rownames(perfiles), "clust"]
  datos %>%
    left_join(perfiles %>% select(all_of(activas), grupo), by = activas) %>%
    mutate(grupo = as.character(grupo))
}

# Los grupos de HCPC vienen numerados en cualquier orden. Los ordenamos por
# prevalencia de alcohol en 12 meses (de menor a mayor) y dejamos al final el
# grupo de tranquilizantes (donde más del 50% los consumió), así son comparables.
ordenar_grupos <- function(df) {
  orden <- df %>%
    group_by(grupo) %>%
    summarise(tranq   = weighted.mean(tranquilizantes == "Tranquilizantes 12m: sí", WPER),
              alcohol = weighted.mean(alcohol == "Alcohol 12m: sí", WPER),
              .groups = "drop") %>%
    mutate(es_tranq = tranq > 0.5) %>%
    arrange(es_tranq, alcohol) %>%
    mutate(grupo_ord = paste0("G", row_number()))
  df %>% left_join(orden %>% select(grupo, grupo_ord), by = "grupo")
}


# ---- 2. Correr todas las especificaciones ---------------------------------
especificaciones <- tribble(
  ~nombre,                                  ~variables,              ~k,
  "Cinco variables, 4 grupos",              "todas",                 4,
  "Cinco variables, 5 grupos (base)",       "todas",                 5,
  "Cinco variables, 6 grupos",              "todas",                 6,
  "Sin intensidad del alcohol, 4 grupos",   "sin_intensidad",        4,
  "Sin intensidad del alcohol, 5 grupos",   "sin_intensidad",        5,
  "Alcohol unificado (sin solapamiento), 5 grupos", "alcohol_unificado", 5
)

resultados <- pmap(especificaciones, function(nombre, variables, k) {
  activas <- switch(variables,
                    todas             = activas_todas,
                    sin_intensidad    = activas_sin_intensidad,
                    alcohol_unificado = activas_unificado)
  armar_clusters(datos, activas, k) %>%
    ordenar_grupos() %>%
    mutate(especificacion = nombre, k = k, variables = variables)
})
names(resultados) <- especificaciones$nombre


# ---- 3. Resumen: ¿aparece siempre el grupo de tranquilizantes? -------------
# Para cada especificación: cuántas personas quedan en el grupo de
# tranquilizantes y cómo es (sexo, edad), y cuánto se parece la partición a
# la original del script 05 (índice de Rand ajustado: 1 = idéntica).
resumen_robustez <- imap_dfr(resultados, function(df, nombre) {
  grupo_tranq <- df %>%
    group_by(grupo_ord) %>%
    summarise(tranq = weighted.mean(tranquilizantes == "Tranquilizantes 12m: sí", WPER),
              .groups = "drop") %>%
    filter(tranq > 0.5) %>%
    pull(grupo_ord)

  en_tranq <- df %>% filter(grupo_ord %in% grupo_tranq)

  tibble(
    especificacion      = nombre,
    k                   = df$k[1],
    hay_grupo_tranq     = length(grupo_tranq) > 0,
    pct_pob_grupo_tranq = if (length(grupo_tranq) > 0) 100 * sum(en_tranq$WPER) / sum(df$WPER) else NA_real_,
    pct_mujeres         = if (length(grupo_tranq) > 0) 100 * weighted.mean(en_tranq$sexo == "Mujer", en_tranq$WPER) else NA_real_,
    pct_50_o_mas        = if (length(grupo_tranq) > 0) 100 * weighted.mean(en_tranq$grupo_edad %in% c("Edad 50-65", "Edad 66-75"), en_tranq$WPER) else NA_real_,
    rand_vs_base_05     = indice_rand_ajustado(df$grupo, df$cluster_num, df$WPER)
  )
})

write_csv(resumen_robustez, "output/tablas/t20_robustez_clusters.csv")
print(resumen_robustez)
# DEBERÍAS VER:
#  - En todas las especificaciones hay un grupo de tranquilizantes de alrededor
#    del 7% de la población, con unos dos tercios de mujeres y mayoría de
#    50 años o más (en la entrega 2: 66% mujeres, 62% mayores de 50).
#  - La especificación de 5 grupos con las cinco variables reproduce el
#    resultado del script 05 (índice de Rand ajustado igual a 1).
#  - Con 4 o 6 grupos el índice de Rand baja: el alcohol se reparte distinto.


# ---- 4. Perfiles de consumo con 4, 5 y 6 grupos ----------------------------
perfiles_k <- resultados[c("Cinco variables, 4 grupos",
                           "Cinco variables, 5 grupos (base)",
                           "Cinco variables, 6 grupos")] %>%
  imap_dfr(function(df, nombre) {
    df %>%
      group_by(k, grupo_ord) %>%
      summarise(
        pct_poblacion         = 100 * sum(WPER) / sum(df$WPER),
        `Alcohol 12m`         = weighted.mean(alcohol == "Alcohol 12m: sí", WPER),
        `5+ tragos (30 días)` = weighted.mean(intensidad == "Intensidad: 5 o más tragos", WPER),
        `Tabaco 12m`          = weighted.mean(tabaco == "Tabaco 12m: sí", WPER),
        `Marihuana 12m`       = weighted.mean(marihuana == "Marihuana 12m: sí", WPER),
        `Tranquilizantes 12m` = weighted.mean(tranquilizantes == "Tranquilizantes 12m: sí", WPER),
        .groups = "drop"
      )
  })

write_csv(perfiles_k, "output/tablas/t21_robustez_perfiles.csv")

g_perfiles <- perfiles_k %>%
  pivot_longer(c(`Alcohol 12m`, `5+ tragos (30 días)`, `Tabaco 12m`,
                 `Marihuana 12m`, `Tranquilizantes 12m`),
               names_to = "variable", values_to = "prev") %>%
  mutate(variable = factor(variable, levels = c("Alcohol 12m", "5+ tragos (30 días)",
                                                "Tabaco 12m", "Marihuana 12m",
                                                "Tranquilizantes 12m")),
         grupo_lab = paste0(grupo_ord, " (", coma(pct_poblacion, 0), "%)"),
         k_lab = paste0(k, " grupos")) %>%
  ggplot(aes(x = variable, y = fct_rev(grupo_lab), fill = prev)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(coma(100 * prev, 0), "%"), color = prev > 0.6),
            size = 3, show.legend = FALSE) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", guide = "none") +
  scale_color_manual(values = c("FALSE" = "grey15", "TRUE" = "white")) +
  scale_x_discrete(labels = function(x) str_wrap(x, 12), position = "top") +
  facet_wrap(~ k_lab, ncol = 1, scales = "free_y") +
  labs(
    title    = "Los perfiles de consumo con 4, 5 y 6 grupos",
    subtitle = "Porcentaje de cada grupo que consumió, ponderado. Entre paréntesis, el tamaño del grupo",
    x = NULL, y = NULL, caption = fuente
  ) +
  tema_g11(base_size = 11) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot")

guardar(g_perfiles, "g18_robustez_perfiles_k", w = 10, h = 11)

message("06_robustez_clusters.R terminado: tablas t20 y t21, gráfico g18.")
