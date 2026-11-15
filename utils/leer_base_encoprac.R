# leer_base_encoprac(): lee la base cruda de la ENCoPraC 2022 (texto del INDEC)
# tal como viene, sin modificarla, y pasa los nombres de columnas a mayúsculas.
# Formato según el manual del INDEC: separador "|", decimales con coma,
# texto entre comillas dobles. Se usa en 01_limpieza.R y en 02_descriptivas.R.
leer_base_encoprac <- function(ruta = "raw/base_encoprac_2022.txt") {
  base_cruda <- readr::read_delim(
    ruta,
    delim = "|",
    quote = "\"",
    locale = readr::locale(decimal_mark = ","),
    show_col_types = FALSE,
    guess_max = 20000
  )
  names(base_cruda) <- toupper(names(base_cruda))
  base_cruda
}
