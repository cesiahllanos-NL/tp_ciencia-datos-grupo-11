# estilo_graficos.R: fuente, tema y colores comunes a todos los gráficos del TP,
# para que todos tengan el mismo formato y siempre citen de dónde salen los datos.
# Requiere ggplot2 (lo carga tidyverse en el script que lo llama).

# Texto de la fuente que va en el caption de cada gráfico
fuente <- paste0("Fuente: elaboración propia en base a la ENCoPraC 2022 ",
                 "(INDEC-SEDRONAR). Datos ponderados con el factor de expansión.")

# Colores fijos: la misma categoría tiene el mismo color en todos los gráficos
colores_sexo <- c("Varón" = "#1F3A5F", "Mujer" = "#C2185B")
color_principal <- "#1F3A5F"
color_secundario <- "#4F7CAC"
color_gris <- "#9CA3AF"

# tema_g11(): theme_minimal con título en negrita y la fuente abajo a la izquierda
tema_g11 <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, color = "grey35",
                                           size = base_size * 0.7),
      plot.caption.position = "plot",
      plot.title.position = "plot"
    )
}
