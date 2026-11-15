# ==============================================================================
# 08_graficos_instancia3.R
# TP Ciencia de Datos · Grupo 11 · ENCoPraC 2022 (INDEC-SEDRONAR)
# ------------------------------------------------------------------------------
# QUÉ GENERA (JPG, 300 dpi, en output/graficos/):
#   grupo_11_exploratorio.jpg  -> alcohol vs tabaco por sexo, edad y clima educativo
#   grupo_11_comunicativo.jpg  -> brecha de género en tranquilizantes por edad
#
# QUÉ LEE: input/base_limpia.rds (lo genera 01_limpieza.R). No relee la base
# cruda: la limpieza se hace una sola vez, en el script 01.
#
# LA HISTORIA:
#   Las mujeres consumen tranquilizantes más que los varones en todas las edades
#   (OR común ≈ 2). Por otro lado, a igual edad y sexo, más clima educativo va
#   con más alcohol y menos tabaco. El exploratorio muestra esa segunda relación
#   (P2); el comunicativo cuenta la primera (P1).
# ==============================================================================

library(tidyverse)   # dplyr, ggplot2, readr, scales...
library(ggrepel)     # etiquetas que no se pisan

# Salida: los JPG van a output/graficos (ruta relativa al proyecto)
carpeta_out <- "output/graficos"
dir.create(carpeta_out, recursive = TRUE, showWarnings = FALSE)

if (!file.exists("input/base_limpia.rds")) {
  stop("No encuentro input/base_limpia.rds. Corré antes script/01_limpieza.R ",
       "(o script/00_main.R) con el proyecto tp_g11.Rproj abierto.", call. = FALSE)
}

# Fuente que va en el pie de los dos gráficos
fuente <- "Fuente: elaboración propia en base a la ENCoPraC 2022 (INDEC-SEDRONAR)."

# Colores (se distinguen también en escala de grises)
gris_claro  <- "#B8BCC4"
gris        <- "#6B7280"
gris_oscuro <- "#2F3440"
naranja     <- "#E07B39"
naranja_osc <- "#B4511A"

# Colores por sexo (iguales en los dos gráficos): azul oscuro = varones, rosa fuerte = mujeres
azul        <- "#1F3A5F"
azul_medio  <- "#4F7CAC"
azul_claro  <- "#9DB9D9"
rosa        <- "#C2185B"
rosa_medio  <- "#DE4E93"
rosa_claro  <- "#F5A3C7"
rosa_osc    <- "#A0115A"


# ==============================================================================
# PASO 1. LEER LA BASE LIMPIA
# QUÉ HACE: lee input/base_limpia.rds y adapta las etiquetas de sexo, edad y
# clima educativo a las que usan los gráficos.
# ==============================================================================

base_limpia <- readRDS("input/base_limpia.rds")

dim(base_limpia)    # DEBERÍAS VER: 12062 filas

datos <- base_limpia %>%
  mutate(
    sexo  = fct_recode(sexo, "Varones" = "Varón", "Mujeres" = "Mujer"),
    edad  = grupo_edad,
    # En la base limpia los 7 casos sin dato de clima quedaron como NA
    clima = factor(if_else(is.na(clima), "Sin datos", as.character(clima)),
                   levels = c("Sin datos", "Bajo", "Medio", "Alto"))
  )

# Chequeo contra la segunda entrega: tranquilizantes, % ponderado por sexo
datos %>%
  group_by(sexo) %>%
  summarise(pct = round(100 * weighted.mean(P1A_TR, WPER, na.rm = TRUE), 1))
# DEBERÍAS VER: Varones 4.9 / Mujeres 8.8


# ==============================================================================
# PASO 2. GRÁFICO EXPLORATORIO
# QUÉ MUESTRA: cada punto es un grupo (edad x sexo x clima educativo = 30 grupos),
# en un panel por sexo y con una línea por clima educativo que une las edades.
# Eje X: % que tomó alcohol en los últimos 30 días. Eje Y: % que fumó tabaco.
# Color: clima educativo. Panel: sexo. Tamaño: población que representa.
# Las líneas punteadas son el promedio poblacional y dividen en cuadrantes.
# ==============================================================================

grupos <- datos %>%
  filter(clima != "Sin datos") %>%                  # 7 personas sin dato de clima
  group_by(edad, sexo, clima) %>%
  summarise(
    n        = n(),
    poblacion = sum(WPER) / 1e6,                    # en millones de personas
    alcohol  = weighted.mean(P1M_AL, WPER),
    tabaco   = weighted.mean(P1M_TA, WPER),
    .groups  = "drop"
  )

nrow(grupos)      # DEBERÍAS VER: 30
min(grupos$n)     # DEBERÍAS VER: 135 (el grupo más chico tiene 135 casos)

# Promedios de toda la población (ponderados): marcan los cuadrantes
prom_alcohol <- weighted.mean(datos$P1M_AL, datos$WPER)   # 51,2%
prom_tabaco  <- weighted.mean(datos$P1M_TA, datos$WPER)   # 23,3%

pct <- scales::label_percent(accuracy = 1, decimal.mark = ",")
pct1 <- scales::label_percent(accuracy = 0.1, decimal.mark = ",")

# Orden para que cada línea una las edades de menor a mayor dentro de cada clima
grupos <- grupos %>% arrange(sexo, clima, edad)

# Colores coherentes con el gráfico comunicativo: rosa = mujeres, azul = varones.
# El clima educativo se marca por la oscuridad del tono (más oscuro = clima más alto).
# El tono más fuerte (clima alto) es exactamente el del comunicativo; baja hacia el clima bajo.
niveles_col <- c("Varones: bajo", "Varones: medio", "Varones: alto",
                 "Mujeres: bajo", "Mujeres: medio", "Mujeres: alto")
grupos <- grupos %>%
  mutate(grupo_col = factor(paste0(sexo, ": ", tolower(clima)), levels = niveles_col))

# Etiquetas tipo "pastilla": fondo del color del grupo; texto oscuro si el fondo es claro, blanco si es oscuro
fondos_claros <- c("Varones: bajo", "Mujeres: bajo")
grupos <- grupos %>%
  mutate(txt = if_else(as.character(grupo_col) %in% fondos_claros, "txt_oscuro_sobre_claro", "txt_blanco_sobre_oscuro"))

paleta_clima <- c("Varones: bajo" = azul_claro, "Varones: medio" = azul_medio, "Varones: alto" = azul,
                  "Mujeres: bajo" = rosa_claro, "Mujeres: medio" = rosa_medio, "Mujeres: alto" = rosa)

g_explo <- ggplot(grupos, aes(x = alcohol, y = tabaco)) +
  # líneas de los promedios (cuadrantes)
  geom_vline(xintercept = prom_alcohol, linetype = "dashed", color = "#9AA0AA", linewidth = 0.5) +
  geom_hline(yintercept = prom_tabaco,  linetype = "dashed", color = "#9AA0AA", linewidth = 0.5) +
  # textos de los cuadrantes
  annotate("text", x = 0.175, y = 0.488, label = "Más tabaco,\nmenos alcohol",
           hjust = 0, vjust = 1, size = 3, color = gris, fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 0.825, y = 0.488, label = "Más alcohol\ny más tabaco",
           hjust = 1, vjust = 1, size = 3, color = gris, fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 0.175, y = 0.022, label = "Menos alcohol\ny menos tabaco",
           hjust = 0, vjust = 0, size = 3, color = gris, fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 0.825, y = 0.022, label = "Más alcohol,\nmenos tabaco",
           hjust = 1, vjust = 0, size = 3, color = gris, fontface = "italic", lineheight = 0.9) +
  # trayectoria por edad dentro de cada clima educativo (de 16-24 a 66-75)
  geom_path(aes(color = grupo_col, group = grupo_col), linewidth = 0.8, alpha = 0.9) +
  # puntos: fill = clima, size = población
  geom_point(aes(fill = grupo_col, size = poblacion), shape = 21,
             color = gris_oscuro, stroke = 0.6, alpha = 0.95) +
  # etiqueta = grupo de edad
  geom_label_repel(aes(label = edad, fill = grupo_col, color = txt),
                   size = 2.8, label.size = 0, label.padding = unit(0.15, "lines"),
                   box.padding = 0.3, point.padding = 0.2,
                   min.segment.length = 0.2, segment.color = gris_claro,
                   max.overlaps = Inf, seed = 123, show.legend = FALSE) +
  facet_wrap(~ sexo) +
  scale_fill_manual(values = paleta_clima,
                    name = "Clima educativo\n(más oscuro = más alto)") +
  scale_color_manual(values = c(paleta_clima,
                                "txt_oscuro_sobre_claro" = gris_oscuro,
                                "txt_blanco_sobre_oscuro" = "white"),
                     guide = "none") +
  scale_size_continuous(range = c(2, 6), breaks = c(0.3, 0.6, 1.2),
                        name = "Población del\ngrupo (millones)",
                        labels = scales::label_number(decimal.mark = ",")) +
  scale_x_continuous(labels = pct, limits = c(0.17, 0.83), breaks = seq(0.2, 0.8, 0.2)) +
  scale_y_continuous(labels = pct, limits = c(0.02, 0.49), breaks = seq(0.1, 0.4, 0.1)) +
  guides(fill  = guide_legend(order = 1, nrow = 2, byrow = TRUE,
                              override.aes = list(shape = 21, size = 3.5)),
         size  = guide_legend(order = 2, nrow = 1,
                              override.aes = list(shape = 21, fill = gris_claro))) +
  labs(
    title    = "Alcohol y tabaco en los últimos 30 días,\nsegún sexo, edad y clima educativo del hogar",
    subtitle = paste0("Cada punto es un grupo de edad; la línea une las edades (16-24 a 66-75) de un mismo clima educativo. Rosa = mujeres, azul = varones.\n",
                      "Líneas punteadas: promedio de la población de 16 a 75 años de 31 aglomerados urbanos (alcohol ",
                      pct1(prom_alcohol), "; tabaco ", pct1(prom_tabaco), ")."),
    x        = "% que tomó alcohol (últimos 30 días)",
    y        = "% que fumó tabaco (últimos 30 días)",
    caption  = paste0(fuente, "\nPrevalencias ponderadas con WPER. Todos los grupos tienen al menos 135 casos.\n",
                      "Se excluyen 7 personas sin dato de clima educativo. Los ejes no comienzan en cero.")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", size = 15, color = gris_oscuro),
    plot.subtitle = element_text(size = 10, color = gris, margin = margin(b = 10)),
    plot.caption  = element_text(size = 8.5, color = gris, hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot",
    strip.text    = element_text(face = "bold", size = 12, hjust = 0, color = gris_oscuro),
    panel.spacing = unit(1, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.35),
    axis.title    = element_text(size = 10, color = gris_oscuro),
    legend.position = "bottom",          # leyendas abajo: los paneles ganan ancho
    legend.box      = "horizontal",
    legend.justification = "left",
    legend.box.just = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.x = unit(0.5, "cm"),
    legend.title  = element_text(size = 8.5, face = "bold"),
    legend.text   = element_text(size = 8)
  )

g_explo   # miralo en el panel Plots antes de guardar

ggsave(
  filename = file.path(carpeta_out, "grupo_11_exploratorio.jpg"),
  plot = g_explo, width = 25, height = 20, units = "cm", dpi = 300,
  bg = "white", quality = 95
)


# ==============================================================================
# PASO 3. GRÁFICO COMUNICATIVO
# MENSAJE: las mujeres consumen más tranquilizantes que los varones en TODAS las
# edades, y la brecha se agranda con los años.
# RECURSOS: color como jerarquía (mujeres en rosa, varones en azul), título
# que afirma el hallazgo, etiquetas directas en lugar de leyenda, sombreado de
# la brecha y una anotación con flecha.
# ==============================================================================

tranq <- datos %>%
  group_by(edad, sexo) %>%
  summarise(p = weighted.mean(P1A_TR, WPER), .groups = "drop") %>%
  mutate(x = as.integer(edad))

brecha <- tranq %>%
  select(edad, x, sexo, p) %>%
  pivot_wider(names_from = sexo, values_from = p) %>%
  mutate(dif = Mujeres - Varones)

round(100 * brecha$dif, 1)
# DEBERÍAS VER: 2.7 1.4 2.1 5.7 11.1  (puntos porcentuales, igual que la segunda entrega)

# Chequeo estadístico para el pie: chi² de independencia dentro de cada edad
p_chi <- datos %>%
  group_by(edad) %>%
  summarise(p = chisq.test(table(sexo, P1A_TR))$p.value)
stopifnot(all(p_chi$p < 0.05))   # si no corta, la brecha es significativa en los 5 grupos

# OR común mujer/varón controlando por edad (Cochran-Mantel-Haenszel).
# Con las columnas en orden 0/1 de P1A_TR, la estimación ya es mujer sobre varón.
or_edad <- unname(mantelhaen.test(table(datos$sexo, datos$P1A_TR, datos$edad))$estimate)
or_edad   # DEBERÍAS VER: aprox. 1.99
or_txt <- str_replace(sprintf("%.1f", or_edad), "\\.", ",")

# Etiquetas de los extremos de cada línea
# Valores de los varones: en 66-75 van abajo a la derecha, donde no llega la línea.
etq_varones <- tranq %>%
  filter(sexo == "Varones") %>%
  mutate(x_etq = if_else(x == 5, x + 0.08, x),
         hj    = if_else(x == 5, 0, 0.5))

# Valores de las mujeres: en los tramos que suben fuerte (50-65 y 66-75) la etiqueta
# va arriba a la izquierda del punto, para que la línea no la pise.
etq_mujeres <- tranq %>%
  filter(sexo == "Mujeres") %>%
  mutate(x_etq = if_else(x >= 3, x - 0.1, x),
         hj    = if_else(x >= 3, 1, 0.5))
extremo <- tranq %>% filter(x == 5)

g_comu <- ggplot(tranq, aes(x = x, y = p)) +
  # sombreado de la brecha
  geom_ribbon(data = brecha, aes(x = x, ymin = Varones, ymax = Mujeres),
              inherit.aes = FALSE, fill = rosa, alpha = 0.15) +
  # líneas y puntos
  geom_line(aes(color = sexo, group = sexo), linewidth = 1.4, lineend = "round") +
  geom_point(aes(color = sexo), size = 3.4) +
  # valores sobre cada punto
  geom_text(data = etq_mujeres,
            aes(x = x_etq, label = pct1(p), hjust = hj), vjust = -0.9, size = 3.6,
            color = rosa_osc, fontface = "bold") +
  geom_text(data = etq_varones,
            aes(x = x_etq, label = pct1(p), hjust = hj), vjust = 2.0, size = 3.6, color = azul) +
  # etiquetas directas (reemplazan la leyenda)
  geom_text(data = extremo, aes(label = sexo, color = sexo),
            hjust = 0, nudge_x = 0.14, size = 4.6, fontface = "bold") +
  # anotación con flecha sobre la brecha
  annotate("text", x = 0.85, y = 0.262, hjust = 0, vjust = 1, size = 3.9,
           color = gris_oscuro, lineheight = 1.05,
           label = paste0("La brecha crece con la edad:\n",
                          "de ", str_replace(sprintf("%.1f", 100 * brecha$dif[1]), "\\.", ","),
                          " puntos porcentuales a los 16-24\n",
                          "a ", str_replace(sprintf("%.1f", 100 * brecha$dif[5]), "\\.", ","),
                          " puntos a los 66-75")) +
  annotate("curve", x = 2.8, y = 0.225, xend = 4.85, yend = 0.200,
           curvature = -0.15, linewidth = 0.5, color = gris_oscuro,
           arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  scale_color_manual(values = c("Varones" = azul, "Mujeres" = rosa), guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = levels(tranq$edad),
                     limits = c(0.75, 5.75), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
                     breaks = seq(0, 0.30, 0.05), limits = c(-0.012, 0.30)) +
  labs(
    title    = "En todas las edades, las mujeres consumen\nmás tranquilizantes que los varones",
    subtitle = paste0("Personas que consumieron tranquilizantes (con o sin receta) en los últimos 12 meses, ",
                      "según sexo y grupo de edad.\nPoblación de 16 a 75 años de 31 aglomerados urbanos, 2022."),
    x        = "Grupo de edad (años)",
    y        = "% que consumió en los últimos 12 meses",
    caption  = paste0(fuente, "\nPrevalencias ponderadas con WPER. La diferencia entre sexos es significativa ",
                      "dentro de cada grupo de edad (chi², p < 0,05).\n",
                      "Controlando por edad, las mujeres tienen ", or_txt,
                      " veces las chances de los varones (OR común de Cochran-Mantel-Haenszel).")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", size = 19, color = gris_oscuro,
                                 lineheight = 1.05, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10.5, color = gris, margin = margin(b = 12)),
    plot.caption  = element_text(size = 8.5, color = gris, hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.4),
    axis.title    = element_text(size = 10.5, color = gris),
    axis.text     = element_text(size = 11, color = gris_oscuro)
  )

g_comu    # miralo en el panel Plots antes de guardar

ggsave(
  filename = file.path(carpeta_out, "grupo_11_comunicativo.jpg"),
  plot = g_comu, width = 22, height = 15, units = "cm", dpi = 300,
  bg = "white", quality = 95
)


# ==============================================================================
# PASO 4. CHECKLIST DE REVISIÓN
# ==============================================================================
# [ ] Abrí los dos JPG desde output/graficos y mirálos al tamaño real.
# [ ] ¿Se lee todo sin hacer zoom? Si no, subí los tamaños de texto.
# [ ] ¿El comunicativo se entiende en 10 segundos sin que expliques nada?
# [ ] Nombres de archivo: grupo_11_exploratorio.jpg y grupo_11_comunicativo.jpg

list.files(carpeta_out, pattern = "grupo_11")
