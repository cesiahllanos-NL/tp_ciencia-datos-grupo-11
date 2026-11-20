# =====================================================================
# 04_chi_cuadrado.R
# Pregunta 1: ¿El consumo de tranquilizantes (últimos 12 meses) es
# independiente del sexo? ¿La brecha se mantiene dentro de cada grupo
# de edad y de nivel educativo?
# Método inferencial: test chi² de independencia.
# =====================================================================

library(tidyverse)
library(survey)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/estilo_graficos.R")   # fuente, tema y colores comunes
source("utils/coma.R")
source("utils/fmt_p.R")
source("utils/pct.R")

base <- readRDS("input/base_limpia.rds")


# ---- Datos -----------------------------------------------------------
d_tr <- base %>%
  filter(!is.na(sexo), !is.na(P1A_TR)) %>%
  mutate(tranq = factor(if_else(P1A_TR == 1, "Consumió", "No consumió"),
                        levels = c("Consumió", "No consumió")))

message("Casos excluidos por NA en sexo o P1A_TR: ", nrow(base) - nrow(d_tr))

# ---- Hipótesis -------------------------------------------------------
# H0: el consumo de tranquilizantes es independiente del sexo.
# H1: el consumo de tranquilizantes depende del sexo.
# Nivel de significación: alfa = 0,05.

# ---- 1. Tablas de contingencia ---------------------------------------
tab_obs <- table(Sexo = d_tr$sexo, Tranquilizantes = d_tr$tranq)
print(tab_obs)

diseno <- svydesign(ids = ~1, weights = ~WPER, data = d_tr)
tab_pond <- svytable(~ sexo + tranq, diseno)
prop_pond <- prop.table(tab_pond, margin = 1)
print(round(100 * prop_pond, 1))

# ---- 2. Test chi² clásico (sobre la muestra) -------------------------
test_chi <- chisq.test(tab_obs, correct = FALSE)
print(test_chi)

# Supuesto: todas las frecuencias esperadas >= 5
print(round(test_chi$expected, 1))
supuesto_ok <- all(test_chi$expected >= 5)
message("¿Todas las frecuencias esperadas >= 5? ", supuesto_ok)

# Residuos estandarizados: |r| > 1,96 indica celdas que se apartan de H0
print(round(test_chi$stdres, 2))

# Tamaño del efecto: V de Cramer (0,1 chico; 0,3 mediano; 0,5 grande)
n <- sum(tab_obs)
v_cramer <- sqrt(unname(test_chi$statistic) / (n * (min(dim(tab_obs)) - 1)))

# ---- 3. Chi² con corrección de Rao-Scott (usa el ponderador WPER) -----
# Chequeo de robustez: el test clásico supone muestreo aleatorio simple,
# esta versión tiene en cuenta la ponderación de la encuesta.
test_rs <- svychisq(~ sexo + tranq, diseno, statistic = "F")
print(test_rs)

resultado_general <- tibble(
  prevalencia_varones = prop_pond["Varón", "Consumió"],
  prevalencia_mujeres = prop_pond["Mujer", "Consumió"],
  n = n,
  chi2 = unname(test_chi$statistic),
  gl = unname(test_chi$parameter),
  p_valor = test_chi$p.value,
  v_cramer = v_cramer,
  esperadas_min = min(test_chi$expected),
  rao_scott_F = unname(test_rs$statistic),
  rao_scott_p = unname(test_rs$p.value)
)
write_csv(resultado_general, "output/tablas/t05_chi2_general.csv")
print(resultado_general)

# ---- 4. Chi² dentro de cada estrato (respuesta a P1) -----------------
chi_por_estrato <- function(datos, var_estrato, nombre_control) {
  datos %>%
    filter(!is.na(.data[[var_estrato]])) %>%
    group_by(estrato = .data[[var_estrato]]) %>%
    group_modify(function(df, key) {
      tab <- table(df$sexo, df$tranq)
      esperadas <- suppressWarnings(chisq.test(tab, correct = FALSE))$expected
      ok <- all(esperadas >= 5)
      # Si alguna esperada es < 5, el p-valor se obtiene por simulación
      t <- if (ok) {
        chisq.test(tab, correct = FALSE)
      } else {
        set.seed(2026)
        chisq.test(tab, simulate.p.value = TRUE, B = 10000)
      }
      prev <- df %>%
        group_by(sexo) %>%
        summarise(p = weighted.mean(tranq == "Consumió", WPER), .groups = "drop")
      # Mismo test con el factor de expansión WPER (Rao-Scott), como en el test general.
      # Es más exigente: dentro de cada estrato hay menos casos y el ponderador
      # agranda la varianza, así que algunos estratos dejan de ser significativos.
      rs <- svychisq(~ sexo + tranq, svydesign(ids = ~1, weights = ~WPER, data = df),
                     statistic = "F")
      tibble(
        n = nrow(df),
        prev_varones = prev$p[prev$sexo == "Varón"],
        prev_mujeres = prev$p[prev$sexo == "Mujer"],
        chi2 = unname(t$statistic),
        p_valor = t$p.value,
        v_cramer = sqrt(unname(t$statistic) / nrow(df)),
        metodo = if (ok) "Chi² asintótico" else "Chi² con p-valor simulado",
        p_rao_scott = unname(rs$p.value)
      )
    }) %>%
    ungroup() %>%
    mutate(control = nombre_control, .before = 1)
}

chi_edad <- chi_por_estrato(d_tr, "grupo_edad", "Grupo de edad")
chi_educ <- chi_por_estrato(d_tr, "nivel_ed3", "Nivel educativo")

tabla_estratos <- bind_rows(
  chi_edad %>% mutate(estrato = as.character(estrato)),
  chi_educ %>% mutate(estrato = as.character(estrato))
) %>%
  mutate(brecha_pp = 100 * (prev_mujeres - prev_varones),
         significativo = p_valor < 0.05,
         # Son 8 tests: corregimos por comparaciones múltiples con el método de Holm
         # (menos conservador que Bonferroni y con el mismo control del error global)
         p_holm = p.adjust(p_valor, method = "holm"),
         significativo_holm = p_holm < 0.05)
write_csv(tabla_estratos, "output/tablas/t06_chi2_por_estrato.csv")
print(tabla_estratos, width = Inf)

# Resumen conjunto controlando por edad: test de Cochran-Mantel-Haenszel
# (sigue siendo un test chi² de independencia, pero estratificado).
cmh_edad <- mantelhaen.test(table(d_tr$sexo, d_tr$tranq, d_tr$grupo_edad) * 1)
cmh_educ <- mantelhaen.test(table(d_tr$sexo, d_tr$tranq, d_tr$nivel_ed3) * 1)
print(cmh_edad)
print(cmh_educ)

tabla_cmh <- tibble(
  control = c("Grupo de edad", "Nivel educativo"),
  chi2_cmh = c(unname(cmh_edad$statistic), unname(cmh_educ$statistic)),
  p_valor = c(cmh_edad$p.value, cmh_educ$p.value),
  # OR común: chances de consumir de varones respecto de mujeres
  or_comun_varon_vs_mujer = c(unname(cmh_edad$estimate), unname(cmh_educ$estimate))
) %>%
  mutate(or_comun_mujer_vs_varon = 1 / or_comun_varon_vs_mujer)
write_csv(tabla_cmh, "output/tablas/t07_cmh.csv")
print(tabla_cmh)

# Chequeo de robustez: modelo logístico conjunto y ponderado.
# El CMH controla por edad y por educación de a una variable y sobre la muestra.
# Acá se controlan las dos a la vez y se usa el ponderador WPER (svyglm).
d_modelo <- d_tr %>% mutate(consumio = as.numeric(P1A_TR == 1))
diseno_modelo <- svydesign(ids = ~1, weights = ~WPER, data = d_modelo)
modelo_conjunto <- svyglm(consumio ~ sexo + grupo_edad + nivel_ed3,
                          design = diseno_modelo, family = quasibinomial())
coef_mujer <- summary(modelo_conjunto)$coefficients["sexoMujer", ]
ic_mujer   <- confint(modelo_conjunto)["sexoMujer", ]

tabla_modelo <- tibble(
  modelo = "Logístico ponderado: sexo + grupo de edad + nivel educativo",
  or_mujer_vs_varon = exp(unname(coef_mujer["Estimate"])),
  ic95_inferior     = exp(unname(ic_mujer[1])),
  ic95_superior     = exp(unname(ic_mujer[2])),
  p_valor           = unname(coef_mujer["Pr(>|t|)"])
)
write_csv(tabla_modelo, "output/tablas/t25_modelo_logistico_ponderado.csv")
print(tabla_modelo)

# ---- 5. Gráficos -----------------------------------------------------

# 5a. General
df_general <- as.data.frame(prop_pond) %>%
  filter(tranq == "Consumió") %>%
  rename(prev = Freq)

g_general <- ggplot(df_general, aes(x = sexo, y = prev, fill = sexo)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = pct(prev)), vjust = -0.4, size = 5) +
  scale_fill_manual(values = colores_sexo, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(coma(100 * x, 0), "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Consumo de tranquilizantes en los últimos 12 meses, según sexo",
       subtitle = paste0("χ² = ", coma(resultado_general$chi2, 1),
                         " (1 gl), ", fmt_p(resultado_general$p_valor),
                         " · V de Cramer = ", coma(v_cramer, 3),
                         " · Rao-Scott: ", fmt_p(resultado_general$rao_scott_p)),
       caption = fuente, x = NULL, y = "Prevalencia (ponderada)") +
  tema_g11(base_size = 13)
guardar(g_general, "g03_chi2_tranquilizantes_sexo")

# 5b. Por estrato
orden_estratos <- c(levels(d_tr$grupo_edad), levels(d_tr$nivel_ed3))

df_estratos <- tabla_estratos %>%
  select(control, estrato, Varón = prev_varones, Mujer = prev_mujeres, p_valor) %>%
  pivot_longer(c(Varón, Mujer), names_to = "sexo", values_to = "prev") %>%
  mutate(sexo = factor(sexo, levels = c("Varón", "Mujer")),
         estrato = factor(estrato, levels = orden_estratos))

etiquetas_p <- tabla_estratos %>%
  mutate(estrato = factor(estrato, levels = orden_estratos),
         y = pmax(prev_varones, prev_mujeres) + 0.03,
         etiqueta = fmt_p(p_valor))

g_estratos <- ggplot(df_estratos, aes(x = estrato, y = prev, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(data = etiquetas_p, aes(x = estrato, y = y, label = etiqueta),
            inherit.aes = FALSE, size = 3.3) +
  facet_wrap(~ control, scales = "free_x",
             labeller = labeller(control = c("Grupo de edad" = "Según grupo de edad",
                                             "Nivel educativo" = "Según nivel educativo"))) +
  scale_fill_manual(values = colores_sexo, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  scale_y_continuous(labels = function(x) paste0(coma(100 * x, 0), "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Brecha por sexo en el consumo de tranquilizantes dentro de cada grupo",
       subtitle = "Prevalencia ponderada en los últimos 12 meses y p-valor del test χ² en cada estrato",
       caption = fuente, x = NULL, y = NULL) +
  tema_g11(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))
guardar(g_estratos, "g04_chi2_por_estrato", w = 11, h = 6)

message("Listo: tablas t05-t07 y gráficos g03-g04.")
