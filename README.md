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

**Hipótesis central:** *COMPLETAR entre los tres integrantes (específica y contrastable).*

El trabajo es descriptivo y **no tiene carácter causal**.

## Base de datos

- **Fuente:** [ENCoPraC 2022, INDEC-SEDRONAR](https://www.indec.gob.ar) (base usuario). *COMPLETAR el link exacto de descarga.*
- **Período:** 2022. *COMPLETAR periodicidad de publicación según la metodología del INDEC.*
- **Unidad de observación:** una persona de 16 a 75 años por hogar, elegida al azar, en 31 aglomerados urbanos.
- **Tamaño:** 12.062 personas (representan 20,8 millones) y 562 variables originales; se usan 23 (ver `auxiliar/diccionario_variables.csv`).
- Todos los resultados se ponderan con el factor de expansión `WPER`.

## Descripción del análisis

1. **Limpieza** (`01_limpieza.R`): selección de 23 variables, códigos especiales a NA, etiquetas y tratamiento de los tragos (original, winsorizado al p99, logaritmo y categorías).
   **Descriptivas** (`02_descriptivas.R`): estadísticas antes y después de la limpieza, distribución de las variables principales, prevalencias de consumo y comparación entre la muestra completa y la del clustering.
2. **Faltantes y outliers** (`03_faltantes_outliers.R`): saltos de cuestionario vs. no respuesta y diagnóstico de outliers en tragos.
3. **Inferencia** (`04_chi_cuadrado.R`): chi² de independencia entre sexo y tranquilizantes, repetido por grupo de edad y nivel educativo, con Rao-Scott y Cochran-Mantel-Haenszel.
4. **Perfiles de consumo** (`05_clustering.R`): análisis de correspondencias múltiples y clasificación jerárquica de Ward (5 clusters).
5. **Concentración** (`07_gini_theil.R`): Gini, curva de Lorenz y descomposición del Theil de los tragos.
6. **Gráficos de la instancia 3** (`08_graficos_instancia3.R`): el exploratorio (alcohol y tabaco por sexo, edad y clima educativo) y el comunicativo (brecha de género en tranquilizantes).
7. **Pendiente:** robustez de los clusters (`06_robustez_clusters.R`).

## Estructura del repositorio

```
tp_g11/
├── raw/              # Base original del INDEC, sin modificar
├── auxiliar/         # Diccionario de variables
├── input/            # Bases procesadas (.rds), las generan los scripts
├── output/
│   ├── tablas/       # Tablas de resultados (.csv)
│   └── graficos/     # Gráficos (.png)
├── script/           # Código, en orden de ejecución
├── utils/            # Funciones propias (un script por función)
├── tp_g11.Rproj
└── README.md
```

## Cómo reproducir el análisis

1. Clonar o descargar el repositorio.
2. Abrir `tp_g11.Rproj` en RStudio (así todas las rutas son relativas).
3. Instalar los paquetes: `tidyverse`, `scales`, `ggrepel`, `survey`, `FactoMineR`, `factoextra`.
4. Correr `script/00_main.R`, o los scripts de a uno en este orden. Cada uno lee los archivos que dejó el anterior:

| Script | Qué hace | Archivos que genera |
|---|---|---|
| `01_limpieza.R` | Lee `raw/`, limpia y etiqueta | `input/base_limpia.rds`, `input/p99_tragos.rds` |
| `02_descriptivas.R` | Descriptivas antes y después de la limpieza, distribuciones y prevalencias | `output/tablas/t16` a `t19`, `output/graficos/g16`, `g17` |
| `03_faltantes_outliers.R` | Diagnóstico de faltantes y outliers | `output/tablas/t01` a `t04`, `output/graficos/g01`, `g02` |
| `04_chi_cuadrado.R` | Chi², Rao-Scott y CMH | `output/tablas/t05` a `t07`, `output/graficos/g03`, `g04` |
| `05_clustering.R` | ACM y clusters | `input/base_con_clusters.rds`, `output/tablas/t08` a `t13`, `output/graficos/g05` a `g13` |
| `07_gini_theil.R` | Gini, Lorenz y Theil | `output/tablas/t14`, `t15`, `output/graficos/g14`, `g15` |
| `08_graficos_instancia3.R` | Gráficos exploratorio y comunicativo | `output/graficos/grupo_11_exploratorio.jpg`, `grupo_11_comunicativo.jpg` |

Las funciones propias están en `utils/` (`guardar`, `coma`, `fmt_p`, `pct`, `leer_base_encoprac`, `mediana_ponderada`, `percentil_ponderado`, `desvio_ponderado`, `calcular_gini`, `calcular_theil`, `descomponer_theil`) y se cargan con `source()`.

## Conclusiones principales

*COMPLETAR una vez cerrado el análisis.*
