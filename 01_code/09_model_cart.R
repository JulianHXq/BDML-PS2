##########################################################
# 09_model_cart.R
#
# Objetivo: Entrenar variantes de Árboles de clasificación (CART) y generar sus
# predicciones para Kaggle.
#
# Etapa del Problem Set: Etapa 2: Training and Comparing Algorithms.
# Insumos:  03_temp/ (bases de modelación, conjuntos de predictores y
#           particiones de validación cruzada).
# Salidas:  02_outputs/predictions/ (un .csv por variante), una fila por
#           variante en 02_outputs/tables/registro_modelos.csv y los
#           resultados de cada variante en 03_temp/.
##########################################################
#
# Este script se puede correr solo (no requiere el script maestro), siempre
# que antes se hayan corrido los scripts 01 a 05. Así se puede iterar con
# Kaggle sin repetir la preparación de los datos.
#
# Estructura de cada variante (un envío a Kaggle por variante):
#   1. Entrenar con train() usando las particiones de 03_temp/cv_folds.rds.
#   2. Elegir el umbral que maximiza F1 en las predicciones fuera de muestra.
#   3. Predecir probabilidades en el test.
#   4. Guardar el envío con fun_save_submission().
##########################################################

# Preparación -------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, caret, pROC, rpart)

source("01_code/functions/fun_metricas.R")
source("01_code/functions/fun_save_submission.R")

train       <- readRDS("03_temp/train_clean.rds")
test        <- readRDS("03_temp/test_clean.rds")
predictores <- readRDS("03_temp/predictores.rds")
cv_folds    <- readRDS("03_temp/cv_folds.rds")

# Las particiones guardan posiciones de fila: solo son válidas si train tiene
# exactamente los mismos hogares, en el mismo orden, que cuando se crearon.
if (max(unlist(cv_folds)) != nrow(train) ||
    length(unique(unlist(cv_folds))) != nrow(train)) {
  stop("Las particiones no corresponden a train. Vuelva a correr ",
       "05_create_cv_folds.R.", call. = FALSE)
}

# Grilla del parámetro de complejidad (cp). Un cp alto poda mucho (árbol
# pequeño, más sesgo); uno bajo deja crecer el árbol (más varianza). Se
# recorren 40 valores en escala logarítmica entre 0,1 y 0,00001. El límite
# inferior es 0,00001 y no 0,0001 porque, con 165 mil hogares, el cp óptimo
# quedaba en el borde de una grilla que terminaba en 0,0001 y la grilla debe
# contener el óptimo en su interior.
grid_cp <- expand.grid(cp = 10^seq(from = log10(0.1), to = log10(0.00001),
                                   length.out = 40))

# Función de una variante -------------------------------------------------
fun_cart_variante <- function(.nombre_conjunto, .sampling = NULL) {
  #' Entrenar una variante de CART y generar su envío a Kaggle
  #'
  #' @description \code{fun_cart_variante()} entrena un árbol de
  #' clasificación con el conjunto de predictores indicado, elige el cp por
  #' validación cruzada y el umbral que maximiza el F1 en las predicciones
  #' fuera de muestra, guarda el envío a Kaggle y registra la variante. Usa
  #' los objetos train, test, predictores, cv_folds y grid_cp creados en la
  #' preparación de este script.
  #'
  #' @param .nombre_conjunto \code{character}. Nombre de un conjunto de
  #' \code{03_temp/predictores.rds}, por ejemplo "X_5_extendido".
  #' @param .sampling \code{character} o \code{NULL}. Rebalanceo de clases
  #' dentro de cada fold: "up", "down" o NULL (sin rebalanceo).
  #'
  #' @returns \code{list} (invisible) con el modelo, el umbral, la
  #' comparación de umbrales y el nombre del archivo.
  
  X <- predictores[[.nombre_conjunto]]
  formula_cart <- formula(paste0("Pobre ~ ", paste0(X, collapse = " + ")))
  rebalanceo <- ifelse(is.null(.sampling), "sin", .sampling)
  
  # 1. Entrenamiento. El cp se elige con el F1 promedio de los folds. El
  # rebalanceo, si se pide, se aplica solo a los hogares de entrenamiento de
  # cada fold; los de validación conservan la proporción real de pobres, que
  # es la que tendrá el test.
  set.seed(2026)
  modelo <- train(formula_cart,
                  data = train,
                  method = "rpart",
                  trControl = fun_crear_ctrl(cv_folds, .sampling),
                  tuneGrid = grid_cp,
                  metric = "F",
                  maximize = TRUE)
  
  # 2. Umbral que maximiza el F1 con las predicciones fuera de muestra del cp
  # elegido. Con rebalanceo, el árbol se entrena con 50% de pobres y sus
  # probabilidades quedan infladas: el umbral óptimo sube para compensarlo.
  oof <- modelo$pred
  res_umbral <- fun_umbral_f1(oof$obs, oof$Yes)
  
  # Evidencia: umbral de 0,5 frente al umbral elegido, con el número de
  # falsos positivos (leakage) y falsos negativos (undercoverage).
  comparacion <- bind_rows(
    fun_metricas_umbral(oof$obs, oof$Yes, 0.5) |>
      mutate(estrategia = "Umbral 0,5"),
    fun_metricas_umbral(oof$obs, oof$Yes, res_umbral$umbral) |>
      mutate(estrategia = "Umbral que maximiza F1")
  ) |>
    relocate(estrategia)
  print(comparacion)
  
  # 3. Predicción en el test, con el árbol reentrenado con todo train.
  prob_test <- predict(modelo, newdata = test, type = "prob")[["Yes"]]
  
  # 4. Envío. El nombre indica el algoritmo, el conjunto, el rebalanceo, el
  # cp y el umbral. El cp se escribe en notación decimal (sin "e-05"), porque
  # el nombre no puede tener guiones.
  cp_str <- gsub(".", "_", formatC(modelo$bestTune$cp, format = "f",
                                   digits = 6), fixed = TRUE)
  archivo <- paste0("cart_", .nombre_conjunto, "_", rebalanceo,
                    "_cp_", cp_str, "_umbral_",
                    fun_hiperparam_str(res_umbral$umbral, .digits = 2))
  fun_save_submission(test, prob_test, res_umbral$umbral, archivo)
  
  # 5. Registro de la variante. El puntaje público de Kaggle se anota a mano
  # en 00_data/kaggle_scores.csv, con el mismo nombre de archivo.
  metricas_cv <- comparacion |> filter(estrategia == "Umbral que maximiza F1")
  fun_registrar_modelo(tibble(
    archivo = archivo,
    algoritmo = "CART",
    conjunto = .nombre_conjunto,
    n_predictores = length(X),
    hiperparametros = paste0("cp = ", signif(modelo$bestTune$cp, 3),
                             "; rebalanceo = ", rebalanceo),
    umbral = res_umbral$umbral,
    f1_cv = metricas_cv$F1,
    precision_cv = metricas_cv$precision,
    recall_cv = metricas_cv$recall,
    auc_cv = res_umbral$auc,
    fecha = as.character(Sys.Date())
  ))
  
  # Se guardan las predicciones fuera de muestra, las del test, el F1 de
  # cada cp y la importancia de las variables, para la comparación de
  # algoritmos y el análisis del mejor modelo.
  saveRDS(list(oof = oof, prob_test = prob_test, umbral = res_umbral$umbral,
               comparacion = comparacion, resultados_cp = modelo$results,
               cp = modelo$bestTune$cp,
               importancia = modelo$finalModel$variable.importance),
          file.path("03_temp", paste0(archivo, ".rds")))
  
  invisible(list(modelo = modelo, umbral = res_umbral$umbral,
                 comparacion = comparacion, archivo = archivo))
}

# Variantes ---------------------------------------------------------------
# Las tres usan el conjunto extendido y difieren solo en el tratamiento del
# desbalance de clases (20% de hogares pobres): ninguno, upsampling
# (se replican hogares pobres hasta igualar a los no pobres) y downsampling
# (se descartan hogares no pobres hasta igualar a los pobres).
cart_X5     <- fun_cart_variante("X_5_extendido")
cart_X5_up  <- fun_cart_variante("X_5_extendido", .sampling = "up")
cart_X5_dwn <- fun_cart_variante("X_5_extendido", .sampling = "down")

# Escalera de conjuntos con CART (sin rebalanceo), para compararla con la
# escalera del logit. No se suben a Kaggle.
# cart_X4     <- fun_cart_variante("X_4_completo")
# cart_X3     <- fun_cart_variante("X_3_empleo")
# cart_X2     <- fun_cart_variante("X_2_demografia")
# cart_X1     <- fun_cart_variante("X_1_vivienda")
