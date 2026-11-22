# Consumos y prácticas de cuidado en la Argentina urbana (ENCoPraC 2022)

Trabajo Práctico de Ciencia de Datos para Economía y Negocios · FCE-UBA · 2c 2026 · **Grupo 11**

## Integrantes

- Nicolás Rodríguez
- Cesiah Calapiña
- Rodrigo Galarza Romillo

## Objetivo e hipótesis de trabajo

Describir cómo se organiza el consumo de sustancias en la población de 16 a 75 años a partir de tres preguntas:

1. **P1.** ¿La brecha entre varones y mujeres en el consumo de tranquilizantes se mantiene al controlar por edad y nivel educativo?
2. **P2.** ¿Qué perfiles de consumo existen y cómo se relacionan con el clima educativo del hogar, la edad y el sexo?
3. **P3.** ¿Qué tan concentrados están los tragos del día de mayor consumo y cuánto de esa desigualdad explican el sexo, la edad y el clima educativo?

**Hipótesis central:** el consumo de sustancias en la población urbana de 16 a 75 años no es homogéneo: se organiza en perfiles según sexo, edad y clima educativo del hogar. En concreto, (i) los tranquilizantes se concentran en las mujeres y esa brecha se sostiene al controlar por edad y nivel educativo; (ii) a igual edad y sexo, un clima educativo más alto va con más alcohol y menos tabaco; y (iii) aun así, estas variables sociodemográficas explican poco de la intensidad del consumo de alcohol, que se concentra en pocas personas y varía sobre todo dentro de cada grupo.

El trabajo es descriptivo y **no tiene carácter causal**.

## Base de datos

- **Fuente:** Encuesta Nacional sobre Consumos y Prácticas de Cuidado (ENCoPraC) 2022, INDEC y SEDRONAR (base usuario). [Informe de resultados](https://www.argentina.gob.ar/sites/default/files/sedronar_indec_2023_encuesta_nacional_sobre_consumos_y_practicas_de_cuidado_2022_encoprac.pdf), [cuadros](https://www.indec.gob.ar/ftp/cuadros/sociedad/cuadros_encoprac_2022.xlsx) y [cuestionario](https://www.indec.gob.ar/ftp/cuadros/sociedad/cuestionario_encoprac_2022.pdf). La base usuario se descargó el 18/09/2026 de la [página de bases de datos del INDEC](https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos-2) ([archivo ZIP](https://www.indec.gob.ar/ftp/cuadros/menusuperior/encoprac/base_usuario_encoprac2022.zip)).
- **Período:** relevada entre julio y octubre de 2022, sobre la muestra de la Encuesta Permanente de Hogares del tercer trimestre de 2022. Se publicó en agosto de 2023.
- **Periodicidad:** no tiene una periodicidad fija. Es la 7.ª edición del estudio nacional sobre consumo de sustancias del Observatorio Argentino de Drogas (SEDRONAR), una serie con antecedentes desde 1999; esta edición estaba prevista para 2020 y se pospuso por la pandemia.
- **Validación:** las prevalencias calculadas con los scripts coinciden con las del informe oficial (alcohol 84,7% alguna vez, 66,2% en 12 meses y 51,2% en 30 días; tabaco 49,2 / 25,6 / 23,3; tranquilizantes 14,1 / 6,9).
- **Unidad de observación:** una persona de 16 a 75 años por hogar, elegida al azar, en 31 aglomerados urbanos.
- **Tamaño:** 12.062 personas (representan 20,8 millones) y 562 variables originales; se usan 23 (ver `auxiliar/diccionario_variables.csv`).
- Todos los resultados se ponderan con el factor de expansión `WPER`.

## Descripción del análisis

1. **Limpieza** (`01_limpieza.R`): selección de 23 variables, códigos especiales a NA, etiquetas y tratamiento de los tragos (original, winsorizado al p99, logaritmo y categorías).
   **Descriptivas** (`02_descriptivas.R`): estadísticas antes y después de la limpieza, distribución de las variables principales (edad, nivel educativo, consumo de alcohol por edad y sexo, edad de inicio), prevalencias de consumo y comparación entre la muestra completa y la del clustering.
2. **Faltantes y outliers** (`03_faltantes_outliers.R`): saltos de cuestionario vs. no respuesta y diagnóstico de outliers en tragos.
3. **Inferencia** (`04_chi_cuadrado.R`): chi² de independencia entre sexo y tranquilizantes, repetido por grupo de edad y nivel educativo, con Rao-Scott y Cochran-Mantel-Haenszel.
4. **Perfiles de consumo** (`05_clustering.R`): análisis de correspondencias múltiples y clasificación jerárquica de Ward (5 clusters).
5. **Concentración** (`07_gini_theil.R`): Gini, curva de Lorenz y descomposición del Theil de los tragos.
6. **Gráficos de la instancia 3** (`08_graficos_instancia3.R`): el exploratorio (alcohol y tabaco por sexo, edad y clima educativo) y el comunicativo (brecha de género en tranquilizantes).
7. **Robustez de los clusters** (`06_robustez_clusters.R`): el mismo procedimiento con 4 y 6 grupos y sin la variable de intensidad del alcohol.

## Estructura del repositorio

```
tp_g11/
├── raw/              # Base original del INDEC, sin modificar
├── auxiliar/         # Diccionario de variables
├── input/            # Bases procesadas (.rds), las generan los scripts
├── output/
│   ├── tablas/       # Tablas de resultados (.csv)
│   └── graficos/     # Gráficos (.png y .jpg)
├── script/           # Código, en orden de ejecución
├── utils/            # Funciones propias (un script por función)
├── tp_g11.Rproj
└── README.md
```

## Cómo reproducir el análisis

1. Clonar o descargar el repositorio.
2. Abrir `tp_g11.Rproj` en RStudio (así todas las rutas son relativas).
3. Instalar los paquetes: `tidyverse`, `scales`, `ggrepel`, `survey`, `FactoMineR`, `factoextra`. El análisis se corrió con **R 4.6.1** y estas versiones: tidyverse 2.0.0 (ggplot2 4.0.3, dplyr 1.2.1, tidyr 1.3.2, readr 2.2.0), scales 1.4.0, ggrepel 0.9.8, survey 4.5, FactoMineR 2.18 y factoextra 2.2.0. El detalle completo queda en `output/session_info.txt`, que se regenera al correr `00_main.R`.
4. Correr `script/00_main.R`, o los scripts de a uno en este orden. Cada uno lee los archivos que dejó el anterior:

| Script | Qué hace | Archivos que genera |
|---|---|---|
| `01_limpieza.R` | Lee `raw/`, limpia y etiqueta | `input/base_limpia.rds`, `input/p99_tragos.rds` |
| `02_descriptivas.R` | Descriptivas antes y después de la limpieza, distribuciones (edad, educación, alcohol por edad y sexo, edad de inicio) y prevalencias | `output/tablas/t16` a `t19` y `t22` a `t24`, `output/graficos/g16`, `g17` y `g19` a `g22` |
| `03_faltantes_outliers.R` | Diagnóstico de faltantes y outliers | `output/tablas/t01` a `t04`, `output/graficos/g01`, `g02` |
| `04_chi_cuadrado.R` | Chi², Rao-Scott, CMH (con Holm) y modelo logístico ponderado | `output/tablas/t05` a `t07` y `t25`, `output/graficos/g03`, `g04` |
| `05_clustering.R` | ACM y clusters | `input/base_con_clusters.rds`, `output/tablas/t08` a `t13`, `output/graficos/g05` a `g13` |
| `06_robustez_clusters.R` | Clusters con 4, 5 y 6 grupos y sin intensidad del alcohol | `output/tablas/t20`, `t21`, `output/graficos/g18` |
| `07_gini_theil.R` | Gini, Lorenz y Theil | `output/tablas/t14`, `t15`, `output/graficos/g14`, `g15` |
| `08_graficos_instancia3.R` | Gráficos exploratorio y comunicativo | `output/graficos/grupo_11_exploratorio.jpg`, `grupo_11_comunicativo.jpg` |

Las funciones propias están en `utils/` (`guardar`, `estilo_graficos` (fuente, tema y colores comunes a todos los gráficos), `coma`, `fmt_p`, `pct`, `leer_base_encoprac`, `indice_rand_ajustado`, `mediana_ponderada`, `percentil_ponderado`, `desvio_ponderado`, `calcular_gini`, `calcular_theil`, `descomponer_theil`) y se cargan con `source()`.

## Decisiones metodológicas y limitaciones

- **Diseño muestral.** La base pública solo trae el ponderador `WPER`: no incluye variables de estrato ni de unidad primaria de muestreo. Por eso los errores estándar se calculan como si la muestra fuera aleatoria simple con ponderadores (`svydesign(ids = ~1)`) y el efecto de diseño reportado (≈ 4,2) es una aproximación.
- **Tests por estrato.** El chi² por grupo de edad y de nivel educativo, y el Cochran-Mantel-Haenszel, se calculan sobre la muestra; las prevalencias que se muestran al lado están ponderadas. Como son 8 tests, también se informan los p-valores corregidos con el método de Holm (tabla `t06`). La tabla `t06` trae además el chi² ponderado (Rao-Scott) de cada estrato: con el ponderador, la brecha deja de ser significativa en algunos estratos chicos (25-34 y 35-49 años y superior completo), porque cada estrato tiene menos casos. Por eso la conclusión de P1 se apoya en las medidas que usan todos los estratos a la vez (CMH y modelo logístico), no en cada test por separado. Como chequeo, un modelo logístico ponderado con sexo, edad y nivel educativo a la vez (`t25`) da una razón de chances de las mujeres respecto de los varones del mismo orden que el CMH.
- **Valores extremos de los tragos.** El máximo es 135 tragos en un día, que probablemente sea un error al pasar de volumen a tragos y no se puede verificar. Por eso en el clustering se usan categorías (menos de 3, 3 a 4,9 y 5 o más) y en el Gini y el Theil se compara la versión original con la winsorizada. El percentil 99 se calcula sin ponderar porque es un control de calidad de cada registro.
- **Clusters.** La cantidad de grupos la elige el criterio automático de HCPC (5 en esta base). El script `06` repite el análisis con 4 y 6 grupos, sin la variable de intensidad y con el alcohol unificado en una sola variable, porque "alcohol en 12 meses" e "intensidad" comparten información.
- **Códigos especiales.** Se pasan a NA en `01_limpieza.R` según el diccionario de registro del INDEC. En la edad de inicio (`AL_03`), 998 = no recuerda y 999 = no sabe/no contesta (681 casos). En los tragos (`AL19_TRAGOS_TOTAL`), 777 = "no consume de forma excesiva" (182 casos) y 888 = "no se pueden calcular los tragos" (45 casos). El 777 no es no respuesta: son personas que tomaron en el último mes sin consumo excesivo. Como no tienen una cantidad de tragos, quedan fuera del Gini, del Theil y del clustering; la tabla `t19` muestra que excluir los 227 casos casi no cambia el perfil de la muestra, aunque probablemente subrepresente un poco a quienes toman poco.

## Cambios a partir de las devoluciones

| Entrega | Devolución | Qué hicimos |
|---|---|---|
| Instancia 1 | Profundizar la interpretación | Cada resultado se interpreta y se conecta con la hipótesis |
| Instancia 1 | Cuidado con los outliers de tragos | Tres tratamientos (winsorizar, logaritmo, categorizar); el Gini se calcula con y sin winsorizar (`01_limpieza.R`, `07_gini_theil.R`) |
| Instancia 1 | Chi² para P1 y clusters para P2 | Chi² por estrato con Cochran-Mantel-Haenszel (`04_chi_cuadrado.R`), y ACM con clasificación de Ward (`05_clustering.R`) |
| Instancia 2 | Robustez de los clusters | Se repiten con 4, 5 y 6 grupos y con otras variables (`06_robustez_clusters.R`, tablas `t20` y `t21`) |
| Instancia 2 | Quedarse con lo principal | De las preguntas nuevas sumamos solo la concentración (P3); el resto queda como próximos pasos |
| Revisión final | Chequeo propio | Chi² ponderado (Rao-Scott) en cada estrato (`t06`) y código 777 corregido según el diccionario del INDEC |

## Conclusiones principales

- **P1 (tranquilizantes).** La brecha de género se sostiene al controlar por edad y por nivel educativo: las mujeres tienen alrededor del doble de chances de haber consumido tranquilizantes en los últimos 12 meses (OR común ≈ 2,0), con 8,8% contra 4,9% en el total. La diferencia relativa es estable, pero la absoluta es mayor en las edades altas: entre 1,4 y 2,7 puntos porcentuales en los menores de 50 años, 5,6 puntos entre los 50 y los 65 años y 11,1 puntos entre los 66 y los 75 años. El efecto es chico (V de Cramer = 0,09).
- **P2 (perfiles).** A igual edad y sexo, un clima educativo más alto se asocia con más consumo de alcohol y menos de tabaco. El ACM y la clasificación de Ward separan cinco perfiles de consumo; uno de ellos reúne a quienes consumen tranquilizantes (66% mujeres, mayoría de 50 años o más), que es la brecha de P1 vista desde otro método. Ese grupo aparece igual en las seis variantes del análisis de robustez (4, 5 y 6 grupos, sin la intensidad y con el alcohol unificado; tabla `t20`). Hay que leerlo con cuidado: el grupo coincide exactamente con quienes consumieron tranquilizantes, porque en el ACM esa categoría, poco frecuente, pesa mucho y separa a esas personas sola. Lo que cambia según la especificación es cómo se reparten los consumos de alcohol en los demás grupos (índice de Rand ajustado de 0,85 a 0,87 con 4 y 6 grupos, y de 0,20 a 0,56 al cambiar las variables).
- **P3 (concentración).** Entre quienes tomaron alcohol en el último mes, el Gini de los tragos del día de mayor consumo es 0,50 (0,49 al winsorizar los valores extremos, por lo que la decisión sobre los outliers no cambia la conclusión). El 10% que más toma concentra el 39,8% de los tragos. Más del 90% de la desigualdad ocurre dentro de los grupos: el sexo explica el 1,9% del índice de Theil, el clima educativo el 1,2% y la edad el 8,1%.
- **Lectura conjunta.** Las variables sociodemográficas ordenan el consumo (quién consume qué), pero explican poco de su intensidad: la mayor parte de la variación está dentro de cada grupo.
- **Limitaciones.** Son asociaciones, no causas. Los clusters dependen de la cantidad de grupos y de las variables elegidas, y el consumo es autorreportado. La variable de tragos mide el día de mayor consumo en los últimos 30 días, no el consumo total.

## Uso de inteligencia artificial

Usamos **Claude (Anthropic)** como asistente. Todo lo que generó lo revisamos, lo corrimos y lo comparamos con nuestras tablas.

- **Para qué la usamos más:** encontrar y corregir errores en el código de R; mejorar el formato de los gráficos y de las diapositivas; chequear que el README, las tablas y la presentación digan lo mismo; ordenar el repositorio y redactar la documentación; redactar los textos de las diapositivas a partir de nuestras ideas.
- **Para qué la usamos menos:** elegir el tema, las preguntas y la hipótesis; decidir los métodos (los ajustamos con las devoluciones del profesor); decidir qué significan los resultados (la síntesis la revisamos y ajustamos nosotros); validar los datos (comparamos las prevalencias con el informe oficial).
