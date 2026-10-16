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

- **Bloque A, preparación de los datos (scripts 01 a 05):** obtiene los datos, los limpia y une, define los conjuntos de predictores, describe la variable de resultado y crea las particiones de validación cruzada. Se corre una vez y guarda sus resultados en `03_temp/`.
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

Las bases de hogares y personas se unen con la variable `id`. Las variables de ingreso (salarios, pensiones, ingreso total del hogar) solo están en la base de entrenamiento, por lo que no se usan como predictores.

## Construcción de la base de modelación

El script `02_clean_merge_data.R` limpia las bases de personas, las resume a nivel hogar, las une con la base de hogares y guarda el resultado en `03_temp/`:

| Base | Hogares | Columnas |
|:--|--:|:--|
| `03_temp/train_clean.rds` | 164.960 | `Pobre` y 21 predictores |
| `03_temp/test_clean.rds` | 66.168 | `id` y 21 predictores |

Ninguna de las dos tiene valores faltantes. El `id` se elimina de la base de entrenamiento para que ningún modelo lo use como predictor, y se conserva en la de prueba porque es la llave del archivo de Kaggle.

### Predictores

| Variable | Tipo | Definición | Fuente |
|:--|:--|:--|:--|
| **Ubicación y vivienda** | | | |
| `cat_ubicacion` | Factor (25) | Ciudad o zona | `Dominio` |
| `cat_tenencia` | Factor (6) | Propia pagada, propia pagando, arriendo, usufructo, posesión sin título, otra | `P5090` |
| `num_personas_cuarto` | Número | Hacinamiento: personas por cuarto | `Nper / P5000` |
| **Composición del hogar** | | | |
| `bin_head_male` | 0/1 | El jefe del hogar es hombre | `P6020`, `P6050` |
| `num_minors` | Número | Personas menores de 18 años | `P6040` |
| `num_old` | Número | Personas mayores de 65 años | `P6040` |
| `num_edad_trabajar` | Número | Personas de 18 a 65 años | `P6040` |
| `prop_dependencia` | Número | (menores de 18 + mayores de 65) / personas de 18 a 65 | `P6040` |
| `num_personas_ug` | Número | Personas de la unidad de gasto | `P6050` |
| **Trabajo e ingresos** | | | |
| `num_ocupados` | Número | Ocupados de la unidad de gasto | `Oc` |
| `prop_ocupados` | Número | Ocupados / personas de la unidad de gasto | `Oc`, `P6050` |
| `num_inactivos` | Número | Inactivos de 18 a 65 años | `Ina` |
| `prop_inactividad` | Número | Inactivos / personas de 18 a 65 años | `Ina` |
| `num_cotizantes` | Número | Personas de la unidad de gasto que cotizan a pensión | `P6920` |
| `bin_hogar_pensionado` | 0/1 | Alguien de la unidad de gasto recibió pensión el mes pasado | `P7500s2` |
| `num_horas_ug` | Número | Horas semanales que trabaja normalmente la unidad de gasto en el trabajo principal | `P6800` |
| `num_asalariados` | Número | Obreros o empleados (particulares o del gobierno) de la unidad de gasto | `P6430` |
| `num_cuentapropia` | Número | Trabajadores por cuenta propia de la unidad de gasto | `P6430` |
| `cat_pos_jefe` | Factor (9) | Posición ocupacional del jefe, incluido "No ocupado" | `P6430`, `P6050` |
| **Capital humano** | | | |
| `cat_maxEduc` | Factor (6) | Nivel educativo más alto del hogar, de "Ninguno" a "Universitaria" | `P6210` |
| `num_educ_jefe_conyuge` | Número | Nivel educativo promedio del jefe y su cónyuge (si lo hay), de 1 ("Ninguno") a 6 ("Universitaria") | `P6210`, `P6050` |

### Decisiones de construcción

- **Unidad de gasto.** Los conteos de ocupados, cotizantes y pensionados excluyen al servicio doméstico, los pensionistas y los trabajadores que viven en el hogar (`P6050` = 6, 7 y 8), porque no comparten su presupuesto. Así lo define el DANE: en los datos, `Nper - Npersug` es exactamente el número de estas personas, y la pobreza se define comparando el ingreso de la unidad de gasto dividido entre `Npersug` con la línea de pobreza.
- **Tasa de dependencia.** Divide a los dependientes entre las personas en edad de trabajar, que son quienes pueden generar ingreso para sostenerlos. En los hogares sin miembros de 18 a 65 años (6,4% de los hogares de entrenamiento), las tasas de dependencia e inactividad no están definidas y se reemplazan por 0. Estos hogares se identifican con `num_edad_trabajar = 0`.
- **Valores faltantes por diseño de la encuesta.** La pregunta de cotización (`P6920`) solo se hace a los ocupados, y la de pensión (`P7500s2`) solo a quienes reportan recibir arriendos o pensiones (`P7495`). En ambos casos, un valor faltante significa "no" y se codifica como 0.
- **Ciudades.** La base de prueba no tiene hogares de Bogotá. Para que train y test tengan las mismas categorías, la lista de los 25 dominios se fija en la función de hogares.
- **Educación.** Se construye como factor ordenado para tomar el máximo del hogar y luego se convierte en factor sin orden, para que cada nivel entre al modelo como su propia variable dicotómica.
- **Tenencia de la vivienda.** Se conservan las 6 categorías porque tienen niveles de pobreza muy distintos (por ejemplo, 9,8% en "propia, la están pagando" frente a 48,3% en "posesión sin título").
- **Calidad del trabajo.** Además de cuántos trabajan (`num_ocupados`), se mide cuánto y cómo trabajan: horas, asalariados, cuenta propia y posición del jefe. Funcionan como aproximación al ingreso laboral, que no está disponible en la base de prueba. Las preguntas de posición ocupacional (`P6430`) y horas (`P6800`) solo se hacen a los ocupados: un valor faltante significa que la persona no trabaja (0 horas, posición "No ocupado"). En la posición del jefe, los trabajadores sin remuneración (códigos 6 y 7) se unen en una sola categoría. En validación cruzada con un logit, estas variables y la educación del jefe y su cónyuge suben el F1 de 0,655 a 0,681.
- **Educación del jefe y su cónyuge.** Se promedia el nivel educativo de los principales generadores de ingreso del hogar. Como todo hogar tiene exactamente un jefe, la misma regla aplica a todos y la variable nunca queda vacía. Se prefirió frente al promedio de los mayores de 18, que no existe en hogares sin adultos y obligaría a usar una regla distinta para ellos.

## Estructura del código

| Script | Responsabilidad |
|:--|:--|
| `00_rundirectory.R` | Script maestro: corre todo el análisis en orden |
| **Bloque A: datos** | |
| `01_get_data.R` | Obtiene los datos crudos de Kaggle y verifica que estén completos |
| `02_clean_merge_data.R` | Limpia personas y hogares, construye las variables a nivel hogar, une las bases, verifica y guarda las bases limpias |
| `03_build_features.R` | Define, con nombre, los conjuntos de predictores entre los que escogen los modelos |
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
| **Funciones** (`01_code/functions/`) | |
| `fun_preprocess_personas.R` | Limpia la base de personas y crea indicadores por persona |
| `fun_colapse_personas.R` | Resume las personas a nivel hogar |
| `fun_preprocess_hogares.R` | Prepara la base de hogares y la variable de resultado |
| `fun_save_submission.R` | Convierte probabilidades en un archivo validado para Kaggle |

### Igualdad de condiciones entre algoritmos

`03_build_features.R` define varios conjuntos de predictores con nombre. Cada envío a Kaggle puede usar cualquiera de ellos, porque probar distintos conjuntos es parte de la iteración. Para la comparación entre algoritmos, en cambio, solo se ponen lado a lado los envíos que usaron **el mismo conjunto**, de modo que la diferencia de puntaje refleje el algoritmo y no las variables. Por eso cada algoritmo tiene al menos una variante con el conjunto común. Comparar un mismo algoritmo con distintos conjuntos muestra, a su vez, el papel de la ingeniería de variables.

Además, todos los modelos se evalúan sobre las mismas particiones de validación cruzada (`05_create_cv_folds.R`).

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
└── 03_temp/                  # bases limpias y modelos intermedios (no versionados)
```

## Salidas

Todas las salidas se generan automáticamente en `02_outputs/`:

- **Figuras** (`02_outputs/figures/`): visualizaciones generadas por el código.
- **Tablas** (`02_outputs/tables/`): resultados en formato `.tex`.
- **Predicciones** (`02_outputs/predictions/`): archivos `.csv` enviados a Kaggle, con columnas `id` y `pobre` (0 o 1). El nombre indica el algoritmo y sus hiperparámetros.
- **Registro de modelos** (`02_outputs/tables/registro_modelos.csv`): una fila por variante entrenada, con su configuración y su F1 en validación cruzada.

## Software

- R 4.6.1 (versión con la que se desarrolló y probó el código)
- Paquetes: se instalan y cargan automáticamente con `pacman::p_load()`
- Opcional, para la descarga automática: Python y la CLI de Kaggle