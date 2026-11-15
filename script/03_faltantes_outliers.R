# =====================================================================
# 03_faltantes_outliers.R
# Diagnóstico de valores faltantes (saltos vs. no respuesta) y de
# outliers en tragos. Genera tablas y gráficos para el PDF.
# =====================================================================

library(tidyverse)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/coma.R")

base <- readRDS("input/base_limpia.rds")
p99_tragos <- readRDS("input/p99_tragos.rds")


# ---- 1. Faltantes: ¿salto de cuestionario o no respuesta? -------------
consumio_algo_12m <- with(base, (P1A_AL == 1) | (P1A_TA == 1) |
                                (P1A_MA == 1) | (P1A_TR == 1))

tabla_faltantes <- tibble(
  variable = c("AL_03", "AL19_TRAGOS_TOTAL", "CP_01"),
  descripcion = c("Edad de inicio de alcohol",
                  "Tragos en el día que más tomó",
                  "Preocupación por el consumo propio"),
  condicion_salto = c("Nunca tomó alcohol (PV_AL = 0)",
                      "No tomó en los últimos 30 días (P1M_AL = 0)",
                      "No consumió en 12 meses (aprox. con AL, TA, MA, TR)"),
  na_total = c(sum(is.na(base$AL_03)),
               sum(is.na(base$tragos)),
               sum(is.na(base$CP_01))),
  na_por_salto = c(sum(is.na(base$AL_03) & base$PV_AL %in% 0),
                   sum(is.na(base$tragos) & base$P1M_AL %in% 0),
                   sum(is.na(base$CP_01) & consumio_algo_12m %in% FALSE))
) %>%
  mutate(na_no_respuesta = na_total - na_por_salto,
         pct_no_respuesta = 100 * na_no_respuesta / nrow(base))

write_csv(tabla_faltantes, "output/tablas/t01_faltantes.csv")
print(tabla_faltantes)

# Faltantes en las variables que usan el chi² y el clustering
vars_metodos <- c("sexo", "grupo_edad", "nivel_ed3", "clima",
                  "P1A_AL", "P1A_TA", "P1A_MA", "P1A_TR", "tragos_cat")
tabla_na_metodos <- base %>%
  summarise(across(all_of(vars_metodos), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(pct_na = 100 * n_na / nrow(base))
write_csv(tabla_na_metodos, "output/tablas/t02_na_variables_metodos.csv")
print(tabla_na_metodos)
# Decisión: con porcentajes tan bajos de no respuesta, se trabaja con
# casos completos en cada método (se informa cuántos casos se excluyen).

# ---- 2. Outliers en tragos -------------------------------------------
t_bebedores <- base %>% filter(!is.na(tragos))

q <- quantile(t_bebedores$tragos, c(0.25, 0.5, 0.75), na.rm = TRUE)
iqr <- q[3] - q[1]
limite_iqr <- unname(q[3] + 1.5 * iqr)

tabla_outliers <- tibble(
  n_con_dato       = nrow(t_bebedores),
  mediana          = unname(q[2]),
  media            = mean(t_bebedores$tragos),
  media_ponderada  = weighted.mean(t_bebedores$tragos, t_bebedores$WPER),
  maximo           = max(t_bebedores$tragos),
  limite_iqr       = limite_iqr,
  n_sobre_iqr      = sum(t_bebedores$tragos > limite_iqr),
  p99              = p99_tragos,
  n_sobre_p99      = sum(t_bebedores$tragos > p99_tragos),
  media_winsorizada = mean(t_bebedores$tragos_wins)
)
write_csv(tabla_outliers, "output/tablas/t03_outliers_tragos.csv")
print(tabla_outliers)

# Comparación de las tres versiones de la variable
comparacion <- t_bebedores %>%
  select(Original = tragos,
         `Winsorizada (p99)` = tragos_wins,
         `Logaritmo (1 + x)` = tragos_log) %>%
  pivot_longer(everything(), names_to = "version", values_to = "valor") %>%
  mutate(version = factor(version, levels = c("Original", "Winsorizada (p99)",
                                               "Logaritmo (1 + x)")))

g_outliers <- ggplot(comparacion, aes(x = version, y = valor)) +
  geom_boxplot(fill = "#9ecae1", outlier.alpha = 0.4, width = 0.5) +
  facet_wrap(~ version, scales = "free") +
  labs(title = "Tragos en el día que más tomó: tres tratamientos de los valores extremos",
       subtitle = paste0("Personas que tomaron en los últimos 30 días (n = ",
                         format(nrow(t_bebedores), big.mark = ".", decimal.mark = ","),
                         "). Percentil 99 = ", coma(p99_tragos), " tragos."),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(), strip.text = element_text(face = "bold"))
guardar(g_outliers, "g01_outliers_tragos")

# Distribución de la versión categórica (la usada en el clustering)
dist_tragos_cat <- base %>%
  filter(!is.na(tragos_cat)) %>%
  count(tragos_cat, wt = WPER) %>%
  mutate(pct = n / sum(n))
write_csv(dist_tragos_cat, "output/tablas/t04_tragos_categorica.csv")

g_tragos_cat <- ggplot(dist_tragos_cat, aes(x = tragos_cat, y = pct)) +
  geom_col(fill = "#3182bd", width = 0.6) +
  geom_text(aes(label = paste0(coma(100 * pct), "%")), vjust = -0.4, size = 4.5) +
  scale_y_continuous(labels = function(x) paste0(coma(100 * x, 0), "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Intensidad de consumo de alcohol (versión categórica)",
       subtitle = "Población de 16 a 75 años, ponderado por WPER",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13)
guardar(g_tragos_cat, "g02_tragos_categorica")

message("Listo: tablas t01-t04 y gráficos g01-g02.")
