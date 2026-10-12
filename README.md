# Predicting Poverty in Colombia

## Problem Set 2: predicción de pobreza a nivel de hogar con modelos de clasificación

**Equipo 03:** Julián Herrera, Andrés Silva, Valentina Vera
Big Data y Machine Learning para Economía Aplicada (MECA 4107), Universidad de los Andes, 2026-20

---

Este repositorio contiene el código para predecir si un hogar colombiano es pobre a partir de información de la encuesta de hogares del DANE (Misión para el Empalme de las Series de Empleo, Pobreza y Desigualdad, MESE). Las predicciones se evalúan con el F1 score en la competencia de Kaggle [uniandes-bdml-2026_20-ps2](https://www.kaggle.com/competitions/uniandes-bdml-2026-20-ps-2).

La pregunta que guía el trabajo es: **¿cuál es el mejor modelo para predecir pobreza y qué implica para la política pública?**

## Instrucciones de replicación

Para reproducir todos los resultados, abra R en la carpeta raíz del repositorio y corra:

```r
source("01_code/00_rundirectory.R")
```

Este script maestro llama a los demás scripts en orden, en tres bloques:

- **Bloque A, preparación de los datos (scripts 01 a 05):** obtiene los datos, los limpia y une, define los predictores comunes, describe la variable de resultado y crea las particiones de validación cruzada. Se corre una vez y guarda sus resultados en `03_temp/`.
- **Bloque B, modelos (scripts 06 a 12):** un script por algoritmo. Cada uno entrena varias variantes y guarda una predicción para Kaggle por variante.
- **Bloque C, análisis (scripts 13 a 15):** compara los algoritmos, analiza el mejor modelo y discute sus implicaciones de política. Produce las figuras y tablas de las presentaciones.

### Datos

Los datos **no** están en el repositorio: la plantilla del curso pide no versionar datos crudos y uno de los archivos supera el límite de 100 MB de GitHub. El script `01_get_data.R` los obtiene así, usando la primera opción que funcione:

1. Si los CSV ya están en `00_data/raw/`, los usa.
2. Si el zip de la competencia está en `00_data/raw/`, lo descomprime.
3. Si la [CLI de Kaggle](https://github.com/Kaggle/kaggle-api) está instalada y configurada, descarga el zip y lo descomprime.
4. Si nada de lo anterior es posible, se detiene con un mensaje que explica cómo obtener los datos.

**Opción automática (recomendada).** Configure la CLI de Kaggle una sola vez:

1. Instale la CLI: `pip install kaggle`
2. En kaggle.com, vaya a *Settings > API > Create New Token* y guarde el archivo en `~/.kaggle/kaggle.json`
3. Acepte las [reglas de la competencia](https://www.kaggle.com/competitions/uniandes-bdml-2026-20-ps-2/rules)

Si usa RStudio y R no encuentra la CLI, corra el código desde la terminal: `R CMD BATCH 01_code/00_rundirectory.R`.

**Opción manual.** Descargue el zip desde la [página de datos](https://www.kaggle.com/competitions/uniandes-bdml-2026-20-ps-2/data) (*Download All*) y guárdelo, sin descomprimir, en `00_data/raw/uniandes-bdml-2026-20-ps-2.zip`.

La competencia incluye:

| Archivo | Contenido |
|:--|:--|
| `train_hogares.csv` | Entrenamiento, nivel hogar (164.960 hogares), incluye la variable `Pobre` |
| `train_personas.csv` | Entrenamiento, nivel persona (543.109 personas) |
| `test_hogares.csv` | Prueba, nivel hogar (66.168 hogares) |
| `test_personas.csv` | Prueba, nivel persona (219.644 personas) |
| `sample_submission.csv` | Formato de envío a Kaggle (`id`, `pobre`) |
| `ddi-documentation-spanish-608.pdf` | Diccionario de variables del DANE |

Las bases de hogares y personas se unen con la variable `id`. Varias variables de la base de entrenamiento no están en la de prueba.

## Estructura del código

| Script | Responsabilidad |
|:--|:--|
| `00_rundirectory.R` | Script maestro: corre todo el análisis en orden |
| **Bloque A: datos** | |
| `01_get_data.R` | Obtiene los datos crudos de Kaggle y verifica que estén completos |
| `02_clean_merge_data.R` | Limpia personas y hogares, construye variables a nivel hogar y une las bases |
| `03_build_features.R` | Define los conjuntos de predictores que usan todos los modelos |
| `04_descriptive_analysis.R` | Etapa 1: distribución de la pobreza, desbalance de clases y relación con los predictores |
| `05_create_cv_folds.R` | Crea las particiones de validación cruzada compartidas por todos los modelos |
| **Bloque B: modelos** | |
| `06_model_lpm.R` | Modelo Lineal de Probabilidad |
| `07_model_logit.R` | Regresión logística |
| `08_model_elastic_net.R` | Logit con regularización Elastic Net |
| `09_model_cart.R` | Árboles de clasificación (CART) |
| `10_model_random_forest.R` | Random Forest |
| `11_model_boosting.R` | Boosting |
| `12_model_naive_bayes.R` | Naive Bayes |
| **Bloque C: análisis** | |
| `13_compare_algorithms.R` | Etapa 2: comparación de algoritmos (presentación *compare*) |
| `14_best_model_analysis.R` | Etapa 3: análisis del mejor modelo (presentación *best*) |
| `15_policy_analysis.R` | Etapa 4: filtración vs. subcobertura y viabilidad de una encuesta corta |
| **Funciones** | |
| `functions/fun_save_submission.R` | Convierte probabilidades en un archivo validado para Kaggle |

### Igualdad de condiciones entre algoritmos

Para que la comparación entre algoritmos refleje el algoritmo y no otras decisiones, todos los modelos usan los mismos predictores (`03_build_features.R`) y las mismas particiones de validación cruzada (`05_create_cv_folds.R`).

## Flujo de trabajo para los envíos a Kaggle

Cada script de modelo se puede correr de forma independiente, una vez corridos los scripts 01 a 05. Así, para probar una nueva variante no hay que repetir la preparación de los datos. Cada variante sigue los mismos pasos:

1. Entrenar el modelo con `train()` usando las particiones compartidas.
2. Elegir el umbral de clasificación que maximiza F1 en las predicciones fuera de muestra.
3. Predecir las probabilidades de ser pobre en la base de prueba.
4. Guardar el envío con `fun_save_submission()`, que verifica el formato antes de guardar.

Por ejemplo:

```r
prob_pobre <- predict(modelo_en, newdata = db_test, type = "prob")[["Yes"]]

nombre <- paste0("EN_lambda_", fun_hiperparam_str(modelo_en$bestTune$lambda),
                 "_alpha_", fun_hiperparam_str(modelo_en$bestTune$alpha))

fun_save_submission(.db_test = db_test, .prob = prob_pobre,
                    .umbral = umbral_optimo, .nombre = nombre)
```

`fun_save_submission()` se detiene con un mensaje claro si faltan hogares, hay valores faltantes, el umbral no está entre 0 y 1 o el nombre tiene caracteres no permitidos. Así no se gasta un envío diario de Kaggle en un archivo con errores.

El archivo queda en `02_outputs/predictions/` y se sube en Kaggle con *Submit Prediction* o desde la terminal:

```bash
kaggle competitions submit -c uniandes-bdml-2026-20-ps-2 \
  -f 02_outputs/predictions/NOMBRE.csv -m "Descripción de la variante"
```

**Nombres de los archivos.** Siguen la convención de la competencia: el algoritmo y sus hiperparámetros, con guiones bajos en lugar de puntos (por ejemplo, `EN_lambda_0_001_alpha_0_5.csv`).

**Registro de puntajes.** El puntaje público solo se ve en Kaggle, así que se anota a mano en `00_data/kaggle_scores.csv`, un insumo versionado con el nombre del archivo, el algoritmo, el puntaje, la fecha, el integrante que lo envió y una descripción. El script `13_compare_algorithms.R` lo lee para comparar los modelos.

## Estructura del repositorio

```
BDML-PS2/
├── README.md
├── 00_data/
│   ├── raw/                  # datos de Kaggle (no versionados)
│   └── kaggle_scores.csv     # puntajes públicos de cada envío
├── 01_code/
│   ├── 00_rundirectory.R     # script maestro
│   ├── 01_ a 05_*.R          # bloque A: datos
│   ├── 06_ a 12_model_*.R    # bloque B: un script por algoritmo
│   ├── 13_ a 15_*.R          # bloque C: análisis
│   └── functions/            # funciones reutilizables
├── 02_outputs/
│   ├── figures/
│   ├── tables/
│   └── predictions/          # archivos .csv enviados a Kaggle
└── 03_temp/                  # datos y modelos intermedios (no versionados)
```

## Salidas

Todas las salidas se generan automáticamente en `02_outputs/`:

- **Figuras** (`02_outputs/figures/`): visualizaciones generadas por el código.
- **Tablas** (`02_outputs/tables/`): resultados en formato `.tex`.
- **Predicciones** (`02_outputs/predictions/`): archivos `.csv` enviados a Kaggle, con columnas `id` y `pobre` (0 o 1). El nombre indica el algoritmo y sus hiperparámetros.
- **Registro de modelos** (`02_outputs/tables/registro_modelos.csv`): una fila por variante entrenada, con su configuración y su F1 en validación cruzada.

## Software

- R (versión 4.3 o superior)
- Paquetes: se instalan y cargan automáticamente con `pacman::p_load()`
- Opcional, para la descarga automática: Python y la CLI de Kaggle
