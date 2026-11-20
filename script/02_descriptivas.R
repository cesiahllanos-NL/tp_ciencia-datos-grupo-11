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
# Qué guarda: output/tablas/t16 a t19 y t22 a t24, y output/graficos/g16, g17 y g19 a g22.
# Todo se pondera con WPER, salvo donde se indica "sin ponderar".
# =====================================================================

library(tidyverse)
library(scales)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/estilo_graficos.R")   # fuente, tema y colores comunes
source("utils/coma.R")
source("utils/pct.R")
source("utils/leer_base_encoprac.R")
source("utils/mediana_ponderada.R")
source("utils/percentil_ponderado.R")
source("utils/desvio_ponderado.R")

dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

base       <- readRDS("input/base_limpia.rds")
base_cruda <- leer_base_encoprac("raw/base_encoprac_2022.txt")



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
  tema_g11(base_size = 11) +
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
  tema_g11(base_size = 11) +
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


# ---- 6. Distribuciones de las variables principales -------------------------
# Edad, nivel educativo, consumo mensual de alcohol por edad y sexo, y edad de
# inicio del consumo de alcohol. Todo ponderado con WPER.
col_varon  <- "#1F3A5F"   # mismos colores que los gráficos de la instancia 3
col_mujer  <- "#C2185B"
col_neutro <- "#4F7CAC"

# 6a. Edad ---------------------------------------------------------------------
mediana_edad <- mediana_ponderada(base$EDAD_SEL, base$WPER)

g_edad <- ggplot(base, aes(x = EDAD_SEL, weight = WPER)) +
  geom_histogram(binwidth = 5, boundary = 16, closed = "left",
                 fill = col_neutro, color = "white") +
  geom_vline(xintercept = mediana_edad, linetype = "dashed", color = "grey30") +
  annotate("text", x = mediana_edad + 1, y = Inf, vjust = 2, hjust = 0, size = 3.6,
           color = "grey20", label = paste0("Mediana: ", coma(mediana_edad, 0), " años")) +
  scale_x_continuous(breaks = seq(20, 70, 10)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1, decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(xlim = c(16, 76)) +
  labs(title    = "Edad de la población de 16 a 75 años",
       subtitle = "Tramos de 5 años, ponderado",
       x = "Edad (años)", y = "Personas (millones)", caption = fuente) +
  tema_g11(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title.position = "plot")

guardar(g_edad, "g19_edad_distribucion", w = 10, h = 5.4)

# 6b. Nivel educativo ----------------------------------------------------------
# Codificación de la EPH: 1 primaria incompleta, 2 primaria completa, 3 secundaria
# incompleta, 4 secundaria completa, 5 superior incompleta, 6 superior completa,
# 7 sin instrucción. Se muestran de menor a mayor nivel.
niveles_ed7 <- c("7" = "Sin instrucción", "1" = "Primaria incompleta",
                 "2" = "Primaria completa", "3" = "Secundaria incompleta",
                 "4" = "Secundaria completa", "5" = "Superior incompleta",
                 "6" = "Superior completa")

dist_educ <- base %>%
  mutate(nivel = factor(as.character(NIVEL_EDUCATIVO_SEL),
                        levels = names(niveles_ed7), labels = unname(niveles_ed7))) %>%
  group_by(nivel) %>%
  summarise(n = n(), poblacion = sum(WPER), .groups = "drop") %>%
  mutate(proporcion = poblacion / sum(poblacion))

write_csv(dist_educ, "output/tablas/t22_distribucion_educacion.csv")
print(dist_educ)
# DEBERÍAS VER (ponderado): sin instrucción 0,5% · primaria incompleta 4,4% ·
# primaria completa 12,7% · secundaria incompleta 21,0% · secundaria completa 26,1% ·
# superior incompleta 17,3% · superior completa 17,9%.

g_educ <- ggplot(dist_educ, aes(x = proporcion, y = fct_rev(nivel))) +
  geom_col(fill = col_neutro, width = 0.7) +
  geom_text(aes(label = pct(proporcion)), hjust = -0.15, size = 3.6) +
  scale_x_continuous(labels = label_percent(accuracy = 1, decimal.mark = ","),
                     limits = c(0, max(dist_educ$proporcion) * 1.2),
                     expand = expansion(mult = c(0, 0))) +
  labs(title    = "Nivel educativo de la población de 16 a 75 años",
       subtitle = "Porcentaje de la población, ponderado",
       x = "% de la población", y = NULL, caption = fuente) +
  tema_g11(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title.position = "plot")

guardar(g_educ, "g20_nivel_educativo", w = 10, h = 5.4)

# 6c. Alcohol en los últimos 30 días, por grupo de edad y sexo --------------------
alcohol_edad_sexo <- base %>%
  filter(!is.na(grupo_edad), !is.na(sexo), !is.na(P1M_AL)) %>%
  group_by(grupo_edad, sexo) %>%
  summarise(prevalencia = weighted.mean(P1M_AL, WPER), n = n(), .groups = "drop") %>%
  group_by(grupo_edad) %>%
  mutate(es_mayor = prevalencia == max(prevalencia)) %>%
  ungroup()

write_csv(alcohol_edad_sexo %>% select(-es_mayor), "output/tablas/t23_alcohol_edad_sexo.csv")
print(alcohol_edad_sexo)
# DEBERÍAS VER: los varones están por encima de las mujeres en los cinco grupos.
# Varones 55,4 · 69,2 · 64,2 · 61,9 · 53,7 y mujeres 49,1 · 49,0 · 41,2 · 33,2 · 26,7
# (de 16-24 a 66-75). El máximo es a los 25-34 años.

g_alcohol <- ggplot(alcohol_edad_sexo, aes(x = grupo_edad, y = prevalencia,
                                           color = sexo, group = sexo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = pct(prevalencia), vjust = if_else(es_mayor, -1, 1.9)),
            size = 3.5, show.legend = FALSE) +
  geom_text(data = alcohol_edad_sexo %>% filter(grupo_edad == "66-75"),
            aes(label = if_else(sexo == "Varón", "Varones", "Mujeres")),
            hjust = -0.3, size = 4.2, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Varón" = col_varon, "Mujer" = col_mujer), guide = "none") +
  scale_x_discrete(expand = expansion(add = c(0.3, 1.1))) +
  scale_y_continuous(labels = label_percent(accuracy = 1, decimal.mark = ","),
                     limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  labs(title    = "El consumo mensual de alcohol es mayor entre los varones en todas las edades",
       subtitle = "Porcentaje que tomó alcohol en los últimos 30 días, ponderado",
       x = "Grupo de edad (años)", y = "% que tomó alcohol (30 días)", caption = fuente) +
  tema_g11(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title.position = "plot")

guardar(g_alcohol, "g21_alcohol_edad_sexo", w = 10, h = 5.8)

# 6d. Edad de inicio del consumo de alcohol --------------------------------------
# Solo quienes recuerdan y contestan (los códigos 998/999 son NA desde 01_limpieza.R)
inicio <- base %>% filter(!is.na(AL_03), !is.na(WPER))

tabla_inicio <- tibble(
  n               = nrow(inicio),
  mediana_pond    = mediana_ponderada(inicio$AL_03, inicio$WPER),
  p25_pond        = percentil_ponderado(inicio$AL_03, inicio$WPER, 0.25),
  p75_pond        = percentil_ponderado(inicio$AL_03, inicio$WPER, 0.75),
  minimo          = min(inicio$AL_03),
  maximo          = max(inicio$AL_03),
  casos_menor_10  = sum(inicio$AL_03 < 10)
)
write_csv(tabla_inicio, "output/tablas/t24_edad_inicio_alcohol.csv")
print(tabla_inicio)
# DEBERÍAS VER: 9.308 personas, mediana 17 años (rango intercuartílico 16 a 19),
# mínimo 4, máximo 71 y 28 casos que empezaron antes de los 10 años.

g_inicio <- ggplot(inicio, aes(x = AL_03, weight = WPER)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left",
                 fill = col_neutro, color = "white") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey40") +
  annotate("text", x = 10.5, y = Inf, vjust = 2, hjust = 0, size = 3.6, color = "grey20",
           label = paste0(tabla_inicio$casos_menor_10, " personas empezaron antes de los 10 años")) +
  annotate("text", x = 10.5, y = Inf, vjust = 4.2, hjust = 0, size = 3.6, color = "grey20",
           label = paste0("Mediana: ", coma(tabla_inicio$mediana_pond, 0), " años")) +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1, decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(xlim = c(0, 50)) +
  labs(title    = "Edad de inicio del consumo de alcohol",
       subtitle = "Personas que tomaron alcohol alguna vez y recuerdan cuándo empezaron, ponderado",
       x = "Edad de inicio (años)", y = "Personas (millones)",
       caption = paste0(fuente, "\nSe muestran hasta los 50 años; el máximo es ", tabla_inicio$maximo, ".")) +
  tema_g11(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title.position = "plot")

guardar(g_inicio, "g22_edad_inicio_alcohol", w = 10, h = 5.4)

message("02_descriptivas.R terminado: tablas t16 a t19 y t22 a t24, gráficos g16, g17 y g19 a g22.")
