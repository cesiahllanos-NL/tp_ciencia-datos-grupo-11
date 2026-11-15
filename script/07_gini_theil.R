# =====================================================================
# 07_gini_theil.R
# Tercer método (unidad 3, índices de desigualdad): ¿qué tan concentrados
# están los tragos en el día de mayor consumo? Gini, curva de Lorenz y
# descomposición del Theil entre y dentro de grupos (sexo, edad, clima).
#
# CUIDADO AL INTERPRETAR: la variable es "tragos en el día que más tomó"
# en los últimos 30 días, NO el consumo total. Se calcula solo entre
# quienes tomaron alcohol en el último mes y tienen dato válido.
# Es un método descriptivo: no hay causalidad.
# =====================================================================

library(tidyverse)
library(scales)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/coma.R")
source("utils/pct.R")
source("utils/calcular_gini.R")
source("utils/calcular_theil.R")
source("utils/descomponer_theil.R")

base <- readRDS("input/base_limpia.rds")
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

fuente <- paste0("Fuente: elaboración propia en base a la ENCoPraC 2022 ",
                 "(INDEC-SEDRONAR). Ponderado por WPER.")


# ---- 1. Universo -------------------------------------------------------
# Quienes tomaron en los últimos 30 días y tienen dato válido de tragos
# (se excluyen los códigos 777/888, que 01_limpieza.R dejó como NA).
bebedores <- base |>
  filter(P1M_AL == 1, !is.na(tragos), tragos > 0, !is.na(WPER))

message("Bebedores con dato válido de tragos: ", nrow(bebedores))


# ---- 2. Probar las funciones con casos conocidos ------------------------
# Reparto igual -> Gini 0. Mitad de las unidades con todo -> Gini 0,5.
stopifnot(abs(calcular_gini(rep(10, 100))) < 1e-9)
stopifnot(abs(calcular_gini(c(rep(0, 50), rep(10, 50))) - 0.5) < 1e-9)


# ---- 3. Gini y concentración, con y sin winsorización -------------------
# Comparar con la winsorización al p99 muestra el efecto de la decisión sobre
# los outliers (el máximo es 135 tragos).
concentracion_top <- function(x, w, p) {
  orden <- order(x, decreasing = TRUE)
  x <- x[orden]; w <- w[orden]
  prop_personas <- cumsum(w) / sum(w)
  prop_tragos   <- cumsum(x * w) / sum(x * w)
  prop_tragos[which(prop_personas >= p)[1]]
}

versiones <- list(
  "Original"            = bebedores$tragos,
  "Winsorizada al p99"  = bebedores$tragos_wins
)

tabla_gini <- imap(versiones, function(x, nombre) {
  tibble(
    version = nombre,
    gini    = calcular_gini(x, bebedores$WPER),
    top_1   = concentracion_top(x, bebedores$WPER, 0.01),
    top_5   = concentracion_top(x, bebedores$WPER, 0.05),
    top_10  = concentracion_top(x, bebedores$WPER, 0.10),
    top_20  = concentracion_top(x, bebedores$WPER, 0.20)
  )
}) |> bind_rows()

write_csv(tabla_gini, "output/tablas/t14_gini_tragos.csv")
tabla_gini   # DEBERÍAS VER: Gini cercano a 0,50 en ambas versiones

gini_orig  <- tabla_gini$gini[tabla_gini$version == "Original"]
top10_orig <- tabla_gini$top_10[tabla_gini$version == "Original"]


# ---- 4. Curva de Lorenz --------------------------------------------------
lorenz_curva <- function(x, w, version) {
  tibble(x = x, w = w) |>
    arrange(x) |>
    mutate(prop_personas = cumsum(w) / sum(w),
           prop_tragos   = cumsum(x * w) / sum(x * w)) |>
    mutate(tramo = ntile(prop_personas, 200)) |>
    group_by(tramo) |>
    summarise(prop_personas = max(prop_personas),
              prop_tragos   = max(prop_tragos), .groups = "drop") |>
    add_row(tramo = 0, prop_personas = 0, prop_tragos = 0, .before = 1) |>
    mutate(version = version)
}

curvas <- imap(versiones, function(x, nombre) {
  lorenz_curva(x, bebedores$WPER, nombre)
}) |> bind_rows()

g_lorenz <- ggplot(curvas, aes(x = prop_personas, y = prop_tragos,
                               color = version, linetype = version)) +
  geom_abline(slope = 1, intercept = 0, color = "grey60", linetype = "dashed") +
  geom_line(linewidth = 1) +
  annotate("text", x = 0.05, y = 0.90, hjust = 0, size = 3.6, color = "grey20",
           label = paste0("Gini = ", coma(gini_orig, 2), "\n",
                          "El 10% que más toma concentra\nel ",
                          pct(top10_orig), " de los tragos")) +
  scale_color_manual(values = c("Original" = "#1F3A5F",
                                "Winsorizada al p99" = "#D81B7A")) +
  scale_linetype_manual(values = c("Original" = "solid",
                                   "Winsorizada al p99" = "dashed")) +
  scale_x_continuous(labels = label_percent(accuracy = 1, decimal.mark = ",")) +
  scale_y_continuous(labels = label_percent(accuracy = 1, decimal.mark = ",")) +
  coord_equal() +
  labs(
    title    = "Concentración de los tragos en el día de mayor consumo",
    subtitle = paste0("Personas de 16 a 75 años que tomaron alcohol en los últimos ",
                      "30 días (n = ", format(nrow(bebedores), big.mark = ".", decimal.mark = ","), ").\n",
                      "La winsorización de los valores extremos casi no cambia la curva."),
    x = "Proporción acumulada de personas (de menos a más tragos)",
    y = "Proporción acumulada de los tragos",
    color = NULL, linetype = NULL,
    caption = fuente
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

guardar(g_lorenz, "g14_lorenz_tragos", w = 10, h = 7)


# ---- 5. Theil: desigualdad entre y dentro de los grupos ------------------
# Para comparar las particiones, todas tienen que correr sobre EXACTAMENTE las
# mismas personas: se descartan los que no tienen sexo, edad o clima educativo.
base_comun <- bebedores |>
  filter(!is.na(sexo), !is.na(grupo_edad), !is.na(clima))

message("Muestra para Theil: ", nrow(base_comun), " de ", nrow(bebedores),
        " bebedores (se pierden los que no tienen clima educativo).")

particiones <- list(
  "Sexo"                      = base_comun$sexo,
  "Grupo de edad"             = base_comun$grupo_edad,
  "Clima educativo del hogar" = base_comun$clima
)

# Theil es muy sensible a los valores extremos: se calcula con los tragos originales
# y con los winsorizados al p99, para ver si la conclusión depende de los extremos.
versiones_theil <- list(
  "Original"           = base_comun$tragos,
  "Winsorizada al p99" = base_comun$tragos_wins
)

tabla_theil <- imap(versiones_theil, function(x, version) {
  imap(particiones, function(grupo, nombre) {
    r <- descomponer_theil(x, base_comun$WPER, grupo)
    tibble(version = version, particion = nombre, T_observado = r$T_observado,
           T_entre = r$T_entre, T_dentro = r$T_dentro)
  }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(pct_entre  = T_entre  / T_observado,
         pct_dentro = T_dentro / T_observado)

# Verificación: entre + dentro tiene que dar el Theil total calculado aparte
stopifnot(all(abs(tabla_theil$T_observado -
                    (tabla_theil$T_entre + tabla_theil$T_dentro)) < 1e-8))

write_csv(tabla_theil, "output/tablas/t15_theil_descomposicion.csv")
tabla_theil

g_theil <- tabla_theil |>
  ggplot(aes(x = pct_entre, y = reorder(particion, pct_entre), fill = version)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = pct(pct_entre)), position = position_dodge(width = 0.7),
            hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = c("Original" = "#1F3A5F", "Winsorizada al p99" = "#9CA3AF")) +
  scale_x_continuous(labels = label_percent(accuracy = 1, decimal.mark = ","),
                     limits = c(0, max(tabla_theil$pct_entre) * 1.3)) +
  labs(
    title    = "¿Cuánta desigualdad en los tragos explica cada variable?",
    subtitle = paste0("Porcentaje del índice de Theil que corresponde a diferencias ENTRE grupos; ",
                      "el resto\nocurre DENTRO de los grupos. Mismas ", format(nrow(base_comun), big.mark = ".", decimal.mark = ","),
                      " personas en todas las barras."),
    x = "% del Theil total explicado por diferencias entre grupos",
    y = NULL, fill = NULL,
    caption = fuente
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot")

guardar(g_theil, "g15_theil_descomposicion", w = 10, h = 5.2)

message("07_gini_theil.R terminado: tablas t14 y t15, gráficos g14 y g15.")
