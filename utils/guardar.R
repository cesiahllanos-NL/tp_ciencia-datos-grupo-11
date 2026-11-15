# guardar(): guarda un gráfico de ggplot2 en output/graficos/ como PNG, 300 dpi.
# Argumentos: g = el gráfico; nombre = nombre del archivo sin extensión;
#             w y h = ancho y alto en centímetros.
# Requiere ggplot2 (lo carga tidyverse en el script que la llama).
guardar <- function(g, nombre, w = 10, h = 5.6) {
  dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path("output/graficos", paste0(nombre, ".png")),
                  g, width = w, height = h, dpi = 300, bg = "white")
}
