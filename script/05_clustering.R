# =====================================================================
# 05_clustering.R
# Pregunta 2: ¿Qué perfiles de consumo hay en la población y cómo se
# relacionan con el clima educativo del hogar, la edad y el sexo?
# Método cuantitativo: ACM (análisis de correspondencias múltiples) +
# clasificación jerárquica sobre los componentes (HCPC).
# =====================================================================

library(tidyverse)
library(FactoMineR)
library(factoextra)

# Funciones propias (carpeta utils/)
source("utils/guardar.R")
source("utils/coma.R")

base <- readRDS("input/base_limpia.rds")

si_no <- function(x, etiqueta) {
  factor(if_else(x == 1, paste0(etiqueta, ": sí"), paste0(etiqueta, ": no")),
         levels = paste0(etiqueta, c(": sí", ": no")))
}

# ---- 1. Variables ----------------------------------------------------
# Activas (definen los grupos): consumo en los últimos 12 meses de cada
# sustancia + intensidad de consumo de alcohol (tragos en categorías,
# lo que evita el problema de los outliers).
# Suplementarias (solo describen los grupos): sociodemográficas.
datos <- base %>%
  transmute(
    ID_PER, WPER,
    alcohol      = si_no(P1A_AL, "Alcohol 12m"),
    intensidad   = fct_relabel(tragos_cat, ~ paste("Intensidad:", .x)),
    tabaco       = si_no(P1A_TA, "Tabaco 12m"),
    marihuana    = si_no(P1A_MA, "Marihuana 12m"),
    tranquilizantes = si_no(P1A_TR, "Tranquilizantes 12m"),
    sexo,
    grupo_edad   = fct_relabel(grupo_edad, ~ paste("Edad", .x)),
    clima        = fct_relabel(clima, ~ paste("Clima", .x)),
    nivel_ed3,
    cobertura,
    preocupa_propio = fct_relabel(preocupa_propio, ~ paste("Preocupación propia:", .x)),
    # Se guardan para el análisis descriptivo de P2
    P1M_AL, P1M_TA
  )

activas <- c("alcohol", "intensidad", "tabaco", "marihuana", "tranquilizantes")
suplementarias <- c("sexo", "grupo_edad", "clima", "nivel_ed3",
                    "cobertura", "preocupa_propio")

n_antes <- nrow(datos)
datos <- datos %>%
  filter(if_all(all_of(activas), ~ !is.na(.x))) %>%
  mutate(across(all_of(suplementarias), ~ fct_na_value_to_level(.x, level = "Sin dato")))
message("Casos excluidos por NA en variables activas: ", n_antes - nrow(datos))

# ---- 2. ACM a nivel individuo (para el mapa con suplementarias) -------
X <- droplevels(as.data.frame(datos[, c(activas, suplementarias)]))
res_acm <- MCA(X,
               quali.sup = (length(activas) + 1):ncol(X),
               row.w = datos$WPER,
               ncp = 5, graph = FALSE)

autovalores <- as.data.frame(res_acm$eig) %>%
  rownames_to_column("dimension")
write_csv(autovalores, "output/tablas/t08_acm_autovalores.csv")
print(head(autovalores, 8))

g_scree <- fviz_screeplot(res_acm, addlabels = TRUE, ncp = 8) +
  labs(title = "ACM: porcentaje de inercia explicado por dimensión",
       x = "Dimensión", y = "% de inercia") +
  theme_minimal(base_size = 13)
guardar(g_scree, "g05_acm_screeplot")

g_mapa <- fviz_mca_var(res_acm, choice = "var.cat", repel = TRUE,
                       col.var = "#08519c", col.quali.sup = "#cb181d",
                       labelsize = 3.2) +
  labs(title = "ACM: mapa de categorías (dimensiones 1 y 2)",
       subtitle = "Azul: variables de consumo (activas) · Rojo: sociodemográficas (suplementarias)") +
  theme_minimal(base_size = 12)
guardar(g_mapa, "g06_acm_mapa_categorias", w = 11, h = 7)

# ---- 3. Clasificación jerárquica (HCPC) ------------------------------
# Con 5 variables categóricas, las 12.000 personas se reparten en pocas
# combinaciones de respuestas ("perfiles"). Personas con el mismo perfil
# tienen exactamente las mismas coordenadas, así que se agrupan y se usa
# como peso la suma de sus WPER. El ACM resultante es idéntico al de nivel
# individuo (ver chequeo abajo) y evita calcular una matriz de distancias
# de 12.000 x 12.000.
perfiles <- datos %>%
  group_by(across(all_of(activas))) %>%
  summarise(peso = sum(WPER), n = n(), .groups = "drop") %>%
  as.data.frame()
message("Perfiles distintos de consumo: ", nrow(perfiles))

res_acm_perf <- MCA(droplevels(perfiles[, activas]), row.w = perfiles$peso,
                    ncp = 5, graph = FALSE)

# Chequeo: los autovalores tienen que coincidir con los del ACM individual
if (!isTRUE(all.equal(unname(res_acm$eig[1:5, 1]),
                      unname(res_acm_perf$eig[1:5, 1]), tolerance = 1e-6))) {
  warning("Los autovalores del ACM por perfiles no coinciden con los del ACM individual.")
}

set.seed(2026)
res_hcpc <- HCPC(res_acm_perf, nb.clust = -1, consol = FALSE, graph = FALSE)
# nb.clust = -1: el número de clusters se elige automáticamente donde
# la ganancia de inercia inter-clase cae más fuerte. Si quieren fijarlo
# a mano (por ejemplo 4), cambien a nb.clust = 4.
# consol = FALSE: la consolidación por k-means no usa los pesos de los
# perfiles, así que se deja la partición del árbol de Ward tal cual.

perfiles$cluster <- res_hcpc$data.clust[rownames(perfiles), "clust"]
message("Número de clusters: ", nlevels(perfiles$cluster))

g_dendro <- fviz_dend(res_hcpc, show_labels = FALSE, rect = TRUE,
                      main = "Dendrograma de la clasificación jerárquica (Ward)") +
  theme_minimal(base_size = 12)
guardar(g_dendro, "g07_dendrograma")

# Asignar el cluster a cada persona según su perfil
datos <- datos %>%
  left_join(perfiles %>% select(all_of(activas), cluster), by = activas) %>%
  rename(cluster_num = cluster)

# Nombre de cada cluster según su perfil de consumo (más fácil de leer
# que "Cluster 1", "Cluster 2"; HCPC numera los grupos en otro orden)
nombres <- datos %>%
  group_by(cluster_num) %>%
  summarise(
    tranq = weighted.mean(tranquilizantes == "Tranquilizantes 12m: sí", WPER),
    intensidad_modal = names(which.max(tapply(WPER, intensidad, sum))),
    .groups = "drop"
  ) %>%
  mutate(
    nombre = case_when(
      tranq > 0.5 ~ "Uso de tranquilizantes",
      intensidad_modal == "Intensidad: No tomó en 30 días" ~ "Sin consumo reciente de alcohol",
      intensidad_modal == "Intensidad: Menos de 3 tragos" ~ "Consumo leve",
      intensidad_modal == "Intensidad: 3 a 4,9 tragos" ~ "Consumo moderado",
      TRUE ~ "Consumo intensivo y policonsumo"
    ),
    orden = match(nombre, c("Sin consumo reciente de alcohol", "Consumo leve",
                            "Consumo moderado", "Consumo intensivo y policonsumo",
                            "Uso de tranquilizantes"))
  ) %>%
  arrange(orden, cluster_num) %>%
  mutate(cluster = paste0("C", row_number(), " · ", nombre))
print(nombres)

datos <- datos %>%
  left_join(nombres %>% select(cluster_num, cluster), by = "cluster_num") %>%
  mutate(cluster = factor(cluster, levels = nombres$cluster))
saveRDS(datos, "input/base_con_clusters.rds")

# Mapa de individuos coloreados por cluster
coords <- as.data.frame(res_acm$ind$coord[, 1:2]) %>%
  setNames(c("dim1", "dim2")) %>%
  mutate(cluster = datos$cluster)
ejes <- paste0("Dim ", 1:2, " (", coma(res_acm$eig[1:2, 2]), "%)")

g_ind <- ggplot(coords, aes(dim1, dim2, color = cluster)) +
  geom_jitter(width = 0.04, height = 0.04, alpha = 0.25, size = 1) +
  stat_ellipse(linewidth = 0.8) +
  labs(title = "Personas en el plano del ACM según cluster",
       x = ejes[1], y = ejes[2], color = NULL) +
  theme_minimal(base_size = 13) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
guardar(g_ind, "g08_acm_individuos_clusters")

# ---- 4. Caracterización de los clusters ------------------------------
# 4a. Tamaño (muestra y población ponderada)
tamanio <- datos %>%
  group_by(cluster) %>%
  summarise(n_muestra = n(), poblacion = sum(WPER), .groups = "drop") %>%
  mutate(pct_poblacion = 100 * poblacion / sum(poblacion))
write_csv(tamanio, "output/tablas/t09_tamanio_clusters.csv")
print(tamanio)

# 4b. Prevalencias ponderadas de cada variable activa por cluster
prev_cluster <- datos %>%
  group_by(cluster) %>%
  summarise(
    `Alcohol 12m`        = weighted.mean(alcohol == "Alcohol 12m: sí", WPER),
    `5+ tragos (30 días)` = weighted.mean(intensidad == "Intensidad: 5 o más tragos", WPER),
    `Tabaco 12m`         = weighted.mean(tabaco == "Tabaco 12m: sí", WPER),
    `Marihuana 12m`      = weighted.mean(marihuana == "Marihuana 12m: sí", WPER),
    `Tranquilizantes 12m` = weighted.mean(tranquilizantes == "Tranquilizantes 12m: sí", WPER),
    .groups = "drop"
  )
write_csv(prev_cluster, "output/tablas/t10_prevalencias_por_cluster.csv")

g_heat <- prev_cluster %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "prev") %>%
  mutate(variable = factor(variable, levels = names(prev_cluster)[-1])) %>%
  ggplot(aes(x = variable, y = fct_rev(cluster), fill = prev)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(coma(100 * prev, 0), "%"),
                color = prev > 0.6), size = 4.5, show.legend = FALSE) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b",
                      labels = function(x) paste0(coma(100 * x, 0), "%"),
                      name = "Prevalencia") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, 12)) +
  labs(title = "Perfil de consumo de cada cluster",
       subtitle = "Prevalencias ponderadas por WPER", x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank())
guardar(g_heat, "g09_perfil_consumo_clusters")

# 4c. Composición sociodemográfica de cada cluster
composicion <- function(var) {
  datos %>%
    count(cluster, categoria = .data[[var]], wt = WPER) %>%
    group_by(cluster) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    mutate(variable = var)
}
tabla_comp <- map_dfr(c("sexo", "grupo_edad", "clima", "nivel_ed3"), composicion) %>%
  mutate(categoria = as.character(categoria))
write_csv(tabla_comp, "output/tablas/t11_composicion_clusters.csv")

# Promedio poblacional como referencia
total_pob <- map_dfr(c("sexo", "grupo_edad", "clima", "nivel_ed3"), function(var) {
  datos %>% count(categoria = .data[[var]], wt = WPER) %>%
    mutate(pct = n / sum(n), variable = var, cluster = "Total",
           categoria = as.character(categoria))
})

graf_comp <- function(var, titulo, nombre) {
  df <- bind_rows(tabla_comp, total_pob) %>%
    filter(variable == var, categoria != "Sin dato") %>%
    mutate(cluster = factor(cluster, levels = c(levels(datos$cluster), "Total")),
           categoria = factor(categoria, levels = unique(categoria)))
  g <- ggplot(df, aes(x = cluster, y = pct, fill = categoria)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = ifelse(pct >= 0.06, paste0(coma(100 * pct, 0), "%"), "")),
              position = position_stack(vjust = 0.5), size = 3.6, color = "white") +
    scale_y_continuous(labels = function(x) paste0(coma(100 * x, 0), "%")) +
    scale_fill_brewer(palette = "Set1", name = NULL) +
    labs(title = titulo, subtitle = "Distribución ponderada dentro de cada cluster",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right")
  guardar(g, nombre)
}
graf_comp("clima", "Clima educativo del hogar según cluster", "g10_clusters_clima")
graf_comp("grupo_edad", "Grupo de edad según cluster", "g11_clusters_edad")
graf_comp("sexo", "Sexo según cluster", "g12_clusters_sexo")

# 4d. Categorías que caracterizan a cada cluster (test v de catdes).
# v > 1,96: la categoría está sobrerrepresentada en el cluster;
# v < -1,96: subrepresentada.
desc <- catdes(as.data.frame(datos[, c("cluster", suplementarias)]),
               num.var = 1, row.w = datos$WPER)
tabla_catdes <- imap_dfr(desc$category, function(m, cl) {
  if (is.null(m)) return(NULL)
  as.data.frame(m) %>%
    rownames_to_column("categoria") %>%
    mutate(cluster = cl, .before = 1)
})
write_csv(tabla_catdes, "output/tablas/t12_catdes_clusters.csv")
print(tabla_catdes)

# ---- 5. P2 descriptiva: clima educativo dentro de edad y sexo --------
# Compara personas del mismo sexo y grupo de edad (sin un segundo
# método inferencial, como sugirió la devolución).
p2 <- datos %>%
  filter(!str_detect(clima, "Sin dato"), !str_detect(grupo_edad, "Sin dato"),
         !str_detect(sexo, "Sin dato")) %>%
  mutate(clima = fct_drop(clima), grupo_edad = fct_drop(grupo_edad),
         sexo = fct_drop(sexo)) %>%
  group_by(sexo, grupo_edad, clima) %>%
  summarise(n = n(),
            `Alcohol (30 días)` = weighted.mean(P1M_AL == 1, WPER, na.rm = TRUE),
            `Tabaco (30 días)`  = weighted.mean(P1M_TA == 1, WPER, na.rm = TRUE),
            .groups = "drop")
write_csv(p2, "output/tablas/t13_p2_clima_por_edad_sexo.csv")

g_p2 <- p2 %>%
  pivot_longer(c(`Alcohol (30 días)`, `Tabaco (30 días)`),
               names_to = "sustancia", values_to = "prev") %>%
  mutate(grupo_edad = str_remove(grupo_edad, "Edad ")) %>%
  ggplot(aes(x = grupo_edad, y = prev, color = clima, group = clima)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(sustancia ~ sexo) +
  scale_y_continuous(labels = function(x) paste0(coma(100 * x, 0), "%")) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  labs(title = "Consumo de alcohol y tabaco según clima educativo, por sexo y edad",
       subtitle = "Prevalencias ponderadas en los últimos 30 días",
       x = "Grupo de edad", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))
guardar(g_p2, "g13_p2_clima_edad_sexo", w = 11, h = 6.5)

message("Listo: tablas t08-t13 y gráficos g05-g13.")
