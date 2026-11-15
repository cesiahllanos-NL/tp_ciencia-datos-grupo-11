# =====================================================================
# 02_descriptivas.R
# Estadísticas descriptivas ANTES y DESPUÉS de la limpieza, distribución de
# las variables principales y prevalencias de consumo.
#
# Responde a la consigna de la entrega final (puntos 5 y 7): mostrar las
# descriptivas de la tabla de análisis y el efecto de las decisiones de
# limpieza sobre ellas.
#
# Qué lee:  raw/base_encoprac_2022.txt (solo para el "antes") e
#           input/base_limpia.rds (el "después", lo genera 01_limpieza.R).
# Qué guarda: output/tablas/t16 a t19 y output/graficos/g16 y g17.
# Todo se pondera con WPER, salvo donde se indica "sin ponderar".
# =====================================================================

library(tidyverse)
library(scales)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/coma.R")
source("utils/pct.R")
source("utils/leer_base_encoprac.R")
source("utils/mediana_ponderada.R")
source("utils/percentil_ponderado.R")
source("utils/desvio_ponderado.R")

dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

base       <- readRDS("input/base_limpia.rds")
base_cruda <- leer_base_encoprac("raw/base_encoprac_2022.txt")

fuente <- paste0("Fuente: elaboración propia en base a la ENCoPraC 2022 ",
                 "(INDEC-SEDRONAR). Ponderado por WPER.")


# ---- 1. Variables numéricas: antes y después de la limpieza --------------
# ANTES  = la variable tal como viene en la base, con los códigos especiales
#          (98, 99, 998, 999, 777, 888) leídos como si fueran valores.
# DESPUÉS = los códigos pasan a NA y, para los tragos, se agrega la versión
#          winsorizada al percentil 99 que se usó en 01_limpieza.R.
resumen_numerico <- function(x, w, variable, etapa) {
  x <- as.numeric(x); w <- as.numeric(w)
  ok <- !is.na(x) & !is.na(w)
  tibble(
    variable     = variable,
    etapa        = etapa,
    n            = sum(ok),
    n_na         = sum(!ok),
    media        = mean(x[ok]),
    media_pond   = weighted.mean(x[ok], w[ok]),
    mediana      = median(x[ok]),
    mediana_pond = mediana_ponderada(x[ok], w[ok]),
    desvio       = sd(x[ok]),
    desvio_pond  = desvio_ponderado(x[ok], w[ok]),
    p25          = unname(quantile(x[ok], 0.25)),
    p75          = unname(quantile(x[ok], 0.75)),
    p99          = unname(quantile(x[ok], 0.99)),
    minimo       = min(x[ok]),
    maximo       = max(x[ok])
  )
}

# Control de que la base limpia y la cruda tienen las mismas personas, en el
# mismo orden (hace falta para usar los ponderadores de una con la otra)
stopifnot(nrow(base) == nrow(base_cruda), all(base$ID_PER == base_cruda$ID_PER))

et_antes <- "Antes (códigos especiales como valores)"
et_desp  <- "Después (códigos como NA)"
et_wins  <- "Después, winsorizada al p99"

tabla_antes_despues <- bind_rows(
  resumen_numerico(base_cruda$EDAD_SEL, base_cruda$WPER, "Edad (años)", et_antes),
  resumen_numerico(base$EDAD_SEL,       base$WPER,       "Edad (años)", et_desp),

  resumen_numerico(base_cruda$AL_03, base_cruda$WPER, "Edad de inicio de alcohol (años)", et_antes),
  resumen_numerico(base$AL_03,       base$WPER,       "Edad de inicio de alcohol (años)", et_desp),

  resumen_numerico(base_cruda$AL19_TRAGOS_TOTAL, base_cruda$WPER, "Tragos en el día que más tomó", et_antes),
  resumen_numerico(base$tragos,                  base$WPER,       "Tragos en el día que más tomó", et_desp),
  resumen_numerico(base$tragos_wins,             base$WPER,       "Tragos en el día que más tomó", et_wins)
)

write_csv(tabla_antes_despues, "output/tablas/t16_descriptivas_antes_despues.csv")
print(tabla_antes_despues %>% select(variable, etapa, n, n_na, media, mediana, maximo))
# DEBERÍAS VER (media sin ponderar):
#   Edad: 43,5 antes y después (no cambia, es la variable de control).
#   Edad de inicio: media 84,9 antes (los 998/999 la inflan) y 18,0 después.
#   Tragos: media 36,0 antes (los 777/888 la inflan), 5,4 después, 5,3 winsorizada.
# Las medianas casi no cambian (la mediana es robusta a los extremos).


# ---- 2. Gráfico: el efecto de los códigos especiales sobre los tragos -----
tragos_etapas <- tabla_antes_despues %>%
  filter(variable == "Tragos en el día que más tomó") %>%
  mutate(etapa = factor(etapa, levels = c(et_antes, et_desp, et_wins)))

g_antes_despues <- ggplot(tragos_etapas, aes(x = etapa, y = media_pond, fill = etapa)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = coma(media_pond)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#9CA3AF", "#1F3A5F", "#4F7CAC")) +
  scale_x_discrete(labels = function(x) str_wrap(x, 18)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = paste0("Los códigos especiales llevaban la media de tragos de ",
                      coma(tragos_etapas$media_pond[2]), " a ",
                      coma(tragos_etapas$media_pond[1])),
    subtitle = "Media ponderada de tragos en el día que más tomó, según el tratamiento de los datos",
    x = NULL, y = "Tragos (media ponderada)",
    caption = paste0(fuente, "\nLos códigos 777 y 888 de la base original no son cantidades de tragos.")
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title.position = "plot")

guardar(g_antes_despues, "g17_tragos_antes_despues", w = 10, h = 6)


# ---- 3. Variables categóricas: distribución (después de la limpieza) ------
# Los casos sin dato (clima educativo = 0: 7 casos; cobertura = 9: 5 casos)
# pasaron a NA en la limpieza y acá se muestran como categoría "Sin dato".
dist_categorica <- function(datos, var, nombre) {
  datos %>%
    mutate(categoria = fct_na_value_to_level(.data[[var]], level = "Sin dato")) %>%
    group_by(categoria) %>%
    summarise(n = n(), poblacion = sum(WPER), .groups = "drop") %>%
    mutate(variable    = nombre,
           pct_muestra = n / sum(n),
           pct_pond    = poblacion / sum(poblacion)) %>%
    select(variable, categoria, n, pct_muestra, pct_pond)
}

tabla_categoricas <- bind_rows(
  dist_categorica(base, "sexo",       "Sexo"),
  dist_categorica(base, "grupo_edad", "Grupo de edad"),
  dist_categorica(base, "nivel_ed3",  "Nivel educativo"),
  dist_categorica(base, "clima",      "Clima educativo del hogar"),
  dist_categorica(base, "cobertura",  "Cobertura de salud")
)

write_csv(tabla_categoricas, "output/tablas/t17_distribucion_categoricas.csv")
print(tabla_categoricas)
# DEBERÍAS VER: 12.062 casos en cada variable; clima "Sin dato" = 7 y
# cobertura "Sin dato" = 5.


# ---- 4. Prevalencias de consumo --------------------------------------------
vars_consumo <- tribble(
  ~sustancia,                          ~periodo,            ~variable,
  "Alcohol",                           "Alguna vez",        "PV_AL",
  "Alcohol",                           "Últimos 12 meses",  "P1A_AL",
  "Alcohol",                           "Últimos 30 días",   "P1M_AL",
  "Tabaco",                            "Alguna vez",        "PV_TA",
  "Tabaco",                            "Últimos 12 meses",  "P1A_TA",
  "Tabaco",                            "Últimos 30 días",   "P1M_TA",
  "Marihuana",                         "Alguna vez",        "PV_MA",
  "Marihuana",                         "Últimos 12 meses",  "P1A_MA",
  "Marihuana",                         "Últimos 30 días",   "P1M_MA",
  "Tranquilizantes (con o sin receta)", "Alguna vez",       "PV_TR",
  "Tranquilizantes (con o sin receta)", "Últimos 12 meses", "P1A_TR"
)

prev_consumo <- pmap(vars_consumo, function(sustancia, periodo, variable) {
  x  <- base[[variable]]
  ok <- !is.na(x)
  tibble(sustancia, periodo, n_validos = sum(ok),
         prevalencia = weighted.mean(x[ok], base$WPER[ok]))
}) %>% bind_rows()

write_csv(prev_consumo, "output/tablas/t18_prevalencias_consumo.csv")
print(prev_consumo)
# DEBERÍAS VER: alcohol 84,7% alguna vez, 66,2% en 12 meses y 51,2% en 30 días;
# tabaco 49,2 / 25,6 / 23,3; marihuana 26,3 / 13,8 / 8,1;
# tranquilizantes 14,1% alguna vez y 6,9% en 12 meses.

g_prev <- prev_consumo %>%
  filter(periodo %in% c("Alguna vez", "Últimos 12 meses")) %>%
  mutate(periodo = factor(periodo, levels = c("Alguna vez", "Últimos 12 meses")),
         sustancia = fct_reorder(sustancia, prevalencia, .fun = max)) %>%
  ggplot(aes(x = prevalencia, y = sustancia, fill = periodo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = pct(prevalencia)),
            position = position_dodge(width = 0.75), hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = c("Alguna vez" = "#9CA3AF", "Últimos 12 meses" = "#1F3A5F")) +
  scale_x_continuous(labels = label_percent(accuracy = 1, decimal.mark = ","),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(labels = function(x) str_wrap(x, 16)) +
  labs(
    title    = "Consumo de sustancias: alguna vez en la vida y en los últimos 12 meses",
    subtitle = "Porcentaje de la población de 16 a 75 años de 31 aglomerados urbanos",
    x = "% de la población", y = NULL, fill = NULL, caption = fuente
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), plot.title.position = "plot")

guardar(g_prev, "g16_prevalencias_consumo", w = 10, h = 5.5)


# ---- 5. Muestra completa vs. muestra del clustering -------------------------
# El ACM y el clustering (05_clustering.R) trabajan con casos completos: se
# excluyen quienes no tienen dato válido de tragos (227 personas). Acá se
# compara el perfil de las dos muestras para ver si la exclusión lo altera.
base_cluster <- base %>%
  filter(!is.na(P1A_AL), !is.na(P1A_TA), !is.na(P1A_MA), !is.na(P1A_TR),
         !is.na(tragos_cat))

perfil_muestra <- function(df) {
  w  <- df$WPER
  pp <- function(cond) sum(w[which(cond)]) / sum(w)
  tibble(
    indicador = c("Mujeres", "16 a 24 años", "66 a 75 años",
                  "Clima educativo alto", "Clima educativo bajo",
                  "Alcohol, 12 meses", "Tabaco, 12 meses",
                  "Marihuana, 12 meses", "Tranquilizantes, 12 meses"),
    pct = c(pp(df$sexo == "Mujer"), pp(df$grupo_edad == "16-24"),
            pp(df$grupo_edad == "66-75"), pp(df$clima == "Alto"),
            pp(df$clima == "Bajo"), pp(df$P1A_AL == 1), pp(df$P1A_TA == 1),
            pp(df$P1A_MA == 1), pp(df$P1A_TR == 1))
  )
}

tabla_muestras <- perfil_muestra(base) %>%
  rename(pct_muestra_completa = pct) %>%
  left_join(perfil_muestra(base_cluster) %>% rename(pct_muestra_clustering = pct),
            by = "indicador") %>%
  mutate(diferencia_pp = 100 * (pct_muestra_clustering - pct_muestra_completa))

message("Muestra completa: ", nrow(base), " · muestra del clustering: ",
        nrow(base_cluster), " · excluidos: ", nrow(base) - nrow(base_cluster))
write_csv(tabla_muestras, "output/tablas/t19_muestra_completa_vs_clustering.csv")
print(tabla_muestras)
# DEBERÍAS VER: 12.062 vs 11.835 (227 excluidos). Las diferencias son de uno
# o dos puntos como máximo: alcohol en 12 meses baja de 66,2% a 65,1% y el clima
# alto de 24,2% a 23,7%. Los demás indicadores casi no cambian.

message("02_descriptivas.R terminado: tablas t16 a t19 y gráficos g16 y g17.")
