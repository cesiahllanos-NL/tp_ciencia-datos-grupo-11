# =====================================================================
# 01_limpieza.R
# Lectura, selección de variables, recodificación y tratamiento de
# valores especiales, faltantes y outliers.
# TP Ciencia de Datos · Grupo 11 · Instancia 2 · ENCoPraC 2022
# =====================================================================

library(tidyverse)

# Funciones propias (carpeta utils/)
source("utils/leer_base_encoprac.R")

# ---- Rutas -----------------------------------------------------------
# Cambiá el nombre del archivo por el que descargaste del INDEC.
ruta_base <- "raw/base_encoprac_2022.txt"

dir.create("input", showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

# ---- Lectura ---------------------------------------------------------
# Formato según el manual del INDEC: separador "|", decimales con coma,
# texto entre comillas dobles.
base_raw <- leer_base_encoprac(ruta_base)

# Mismas 23 variables de la primera entrega
vars <- c(
  "ID_PER", "WPER", "SEXO_SEL", "EDAD_SEL", "GRUPO_EDAD_SEL",
  "NIVEL_EDUCATIVO_SEL", "COBERTURA_SEL", "CLIMA_EDUCATIVO",
  "PV_AL", "P1A_AL", "P1M_AL", "PV_TA", "P1A_TA", "P1M_TA",
  "PV_MA", "P1A_MA", "P1M_MA", "PV_TR", "P1A_TR",
  "AL_03", "AL19_TRAGOS_TOTAL", "CP_01", "CP_10"
)

faltan <- setdiff(vars, names(base_raw))
if (length(faltan) > 0) {
  stop("No encuentro estas variables en la base: ",
       paste(faltan, collapse = ", "),
       "\nRevisá los nombres en el diccionario y corregí el vector 'vars'.")
}

base <- base_raw %>% select(all_of(vars))

# ---- Códigos especiales -> NA ----------------------------------------
# Igual que en la primera entrega: 98/99, 998/999, 777/888 = no responde
# o casos especiales según el manual del INDEC. En la base aparecen 998/999
# en AL_03 (681 casos) y 777/888 en AL19_TRAGOS_TOTAL (227 casos).
codigos_especiales <- c(98, 99, 998, 999, 777, 888)

binarias <- c("PV_AL", "P1A_AL", "P1M_AL", "PV_TA", "P1A_TA", "P1M_TA",
              "PV_MA", "P1A_MA", "P1M_MA", "PV_TR", "P1A_TR")

base <- base %>%
  mutate(
    across(c(EDAD_SEL, AL_03, AL19_TRAGOS_TOTAL, WPER), as.numeric),
    across(c(AL_03, AL19_TRAGOS_TOTAL),
           ~ if_else(.x %in% codigos_especiales, NA_real_, .x)),
    # En las binarias solo son válidos 0 (no) y 1 (sí)
    across(all_of(binarias),
           ~ if_else(as.numeric(.x) %in% c(0, 1), as.numeric(.x), NA_real_))
  )

# ---- Etiquetas -------------------------------------------------------
# Códigos verificados contra la base (nivel educativo sigue la codificación
# de la EPH: 7 = sin instrucción). Si aparece un código que no está acá,
# el script avisa con un warning.
etq_sexo <- c("1" = "Varón", "2" = "Mujer")
etq_grupo_edad <- c("1" = "16-24", "2" = "25-34", "3" = "35-49",
                    "4" = "50-65", "5" = "66-75")
etq_nivel_ed <- c("7" = "Sin instrucción", "1" = "Primario incompleto",
                  "2" = "Primario completo", "3" = "Secundario incompleto",
                  "4" = "Secundario completo", "5" = "Superior incompleto",
                  "6" = "Superior completo")
etq_clima <- c("1" = "Bajo", "2" = "Medio", "3" = "Alto")
etq_cobertura <- c("1" = "Obra social o prepaga", "2" = "Solo sistema público")

etiquetar <- function(x, etiquetas) {
  x <- as.character(x)
  sin_etiqueta <- setdiff(unique(na.omit(x)), names(etiquetas))
  if (length(sin_etiqueta) > 0) {
    warning("Códigos sin etiqueta: ", paste(sin_etiqueta, collapse = ", "),
            " (quedan como NA). Revisá el diccionario.", call. = FALSE)
  }
  factor(unname(etiquetas[x]), levels = unname(etiquetas))
}

base <- base %>%
  mutate(
    sexo       = etiquetar(SEXO_SEL, etq_sexo),
    grupo_edad = etiquetar(GRUPO_EDAD_SEL, etq_grupo_edad),
    nivel_ed   = etiquetar(NIVEL_EDUCATIVO_SEL, etq_nivel_ed),
    # Nivel educativo en 3 grupos para tener casos suficientes por estrato
    nivel_ed3  = fct_collapse(
      nivel_ed,
      "Hasta secundario incompleto" = c("Sin instrucción", "Primario incompleto",
                                        "Primario completo", "Secundario incompleto"),
      "Secundario completo o superior incompleto" = c("Secundario completo",
                                                      "Superior incompleto"),
      "Superior completo" = "Superior completo"
    ),
    # CLIMA_EDUCATIVO = 0 (7 casos) y COBERTURA_SEL = 9 (5 casos) son sin dato
    clima      = etiquetar(na_if(CLIMA_EDUCATIVO, 0), etq_clima),
    cobertura  = etiquetar(na_if(COBERTURA_SEL, 9), etq_cobertura)
  )

# ---- Tragos: tratamiento de outliers ---------------------------------
# La devolución pidió cuidar los outliers de tragos por día (máx. 135).
# Generamos tres versiones y la usada en el clustering es la categórica,
# que no se ve afectada por los valores extremos.
p99_tragos <- quantile(base$AL19_TRAGOS_TOTAL, 0.99, na.rm = TRUE)

base <- base %>%
  mutate(
    tragos      = AL19_TRAGOS_TOTAL,
    tragos_wins = pmin(tragos, p99_tragos),   # winsorizado al percentil 99
    tragos_log  = log1p(tragos),              # log(1 + tragos)
    tragos_cat  = case_when(
      P1M_AL == 0  ~ "No tomó en 30 días",
      is.na(tragos) ~ NA_character_,
      tragos < 3   ~ "Menos de 3 tragos",
      tragos < 5   ~ "3 a 4,9 tragos",
      TRUE         ~ "5 o más tragos"
    ),
    tragos_cat = factor(tragos_cat, levels = c("No tomó en 30 días",
                                               "Menos de 3 tragos",
                                               "3 a 4,9 tragos",
                                               "5 o más tragos"))
  )

# ---- Preocupación por el consumo propio ------------------------------
# Códigos: 1 = Sí, 2 = No, 98 = Ns/Nc.
# Los NA son salto de cuestionario: se les asigna "No corresponde".
base <- base %>%
  mutate(
    preocupa_propio = case_when(
      is.na(CP_01) ~ "No corresponde",
      CP_01 == 1   ~ "Sí",
      CP_01 == 2   ~ "No",
      TRUE         ~ "Ns/Nc"
    ),
    preocupa_propio = factor(preocupa_propio)
  )

saveRDS(base, "input/base_limpia.rds")
attr(p99_tragos, "names") <- NULL
saveRDS(p99_tragos, "input/p99_tragos.rds")

message("Base limpia guardada: ", nrow(base), " filas y ", ncol(base), " columnas.")
