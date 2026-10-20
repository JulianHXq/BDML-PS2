##########################################################
# 07_model_logit.R
#
# Objetivo: Entrenar variantes de Regresión logística (Logit) y generar sus
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
pacman::p_load(tidyverse, caret, pROC)

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

ctrl <- fun_crear_ctrl(cv_folds)

# Función de una variante -------------------------------------------------
fun_logit_variante <- function(.nombre_conjunto) {
  #' Entrenar una variante de logit y generar su envío a Kaggle
  #'
  #' @description \code{fun_logit_variante()} entrena un logit con el
  #' conjunto de predictores indicado, elige el umbral que maximiza el F1 en
  #' las predicciones fuera de muestra, guarda el envío a Kaggle y registra
  #' la variante. Usa los objetos train, test, predictores y ctrl creados en
  #' la preparación de este script.
  #'
  #' @param .nombre_conjunto \code{character}. Nombre de un conjunto de
  #' \code{03_temp/predictores.rds}, por ejemplo "X_4_completo".
  #'
  #' @returns \code{list} (invisible) con el modelo, el umbral, la
  #' comparación de umbrales y el nombre del archivo.
  
  X <- predictores[[.nombre_conjunto]]
  formula_logit <- formula(paste0("Pobre ~ ", paste0(X, collapse = " + ")))
  
  # 1. Entrenamiento. El logit no tiene hiperparámetros: la validación
  # cruzada no elige nada, sino que produce, para cada hogar, una
  # probabilidad predicha por un modelo que no lo vio al entrenar. Con esas
  # predicciones se elige el umbral y se estima el F1 fuera de muestra.
  modelo <- train(formula_logit,
                  data = train,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl,
                  metric = "F",
                  maximize = TRUE)
  
  # 2. Umbral. Caret asigna la clase con un umbral de 0,5. Con 20% de hogares
  # pobres ese umbral es muy exigente: el modelo predice "pobre" pocas veces
  # y el recall cae. Se elige el umbral que maximiza el F1 con las
  # predicciones fuera de muestra.
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
  
  # 3. Predicción en el test, con el modelo reentrenado con todo train.
  prob_test <- predict(modelo, newdata = test, type = "prob")[["Yes"]]
  
  # 4. Envío. El logit no tiene hiperparámetros: el nombre indica el
  # algoritmo, el conjunto de predictores y el umbral.
  archivo <- paste0("logit_", .nombre_conjunto, "_umbral_",
                    fun_hiperparam_str(res_umbral$umbral, .digits = 2))
  fun_save_submission(test, prob_test, res_umbral$umbral, archivo)
  
  # 5. Registro de la variante. El puntaje público de Kaggle se anota a mano
  # en 00_data/kaggle_scores.csv, con el mismo nombre de archivo.
  metricas_cv <- comparacion |> filter(estrategia == "Umbral que maximiza F1")
  fun_registrar_modelo(tibble(
    archivo = archivo,
    algoritmo = "Logit",
    conjunto = .nombre_conjunto,
    n_predictores = length(X),
    hiperparametros = "ninguno",
    umbral = res_umbral$umbral,
    f1_cv = metricas_cv$F1,
    precision_cv = metricas_cv$precision,
    recall_cv = metricas_cv$recall,
    auc_cv = res_umbral$auc,
    fecha = as.character(Sys.Date())
  ))
  
  # Se guardan las predicciones fuera de muestra y las del test para la
  # comparación de algoritmos y el análisis del mejor modelo.
  saveRDS(list(oof = oof, prob_test = prob_test, umbral = res_umbral$umbral,
               comparacion = comparacion, coeficientes = coef(modelo$finalModel)),
          file.path("03_temp", paste0(archivo, ".rds")))
  
  invisible(list(modelo = modelo, umbral = res_umbral$umbral,
                 comparacion = comparacion, archivo = archivo))
}

# Variantes ---------------------------------------------------------------
# Primer envío: el conjunto completo.
logit_X4 <- fun_logit_variante("X_4_completo")

# Escalera de conjuntos: cuánto F1 se pierde con una encuesta más corta.
logit_X1 <- fun_logit_variante("X_1_vivienda")
logit_X2 <- fun_logit_variante("X_2_demografia")
logit_X3 <- fun_logit_variante("X_3_empleo")

# Conjunto extendido: X_4_completo más arriendo, demografía detallada,
# desempleo y calidad del empleo.
logit_X5 <- fun_logit_variante("X_5_extendido")
