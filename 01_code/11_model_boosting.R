##########################################################
# 11_model_boosting.R
#
# Objetivo: Entrenar variantes de Boosting y generar sus
# predicciones para Kaggle.
#
# Etapa del Problem Set: Etapa 2: Training and Comparing Algorithms.
# Insumos:  03_temp/ (bases de modelación y particiones de validación cruzada).
# Salidas:  02_outputs/predictions/ (un .csv por variante), una fila por variante en
#           02_outputs/tables/registro_modelos.csv y el modelo en 03_temp/.
# ESTADO: pendiente. Se escribe cuando el tema se vea en clase.
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
# Código pendiente: cargar paquetes, funciones de 01_code/functions/,
# datos de 03_temp/ y particiones de validación cruzada.


######### XGBOOST PILOTO

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, caret, dplyr, xgboost)


##########################################################
# 08_model_xgboost.R
#
# XGBOOST - PILOTO Y SUBMISSION KAGGLE
#
# 1. Cargar y validar datos
# 2. Construir matrices
# 3. Entrenar con CV de 5 folds
# 4. Early stopping interno
# 5. Seleccionar umbral mediante F1 OOF
# 6. Entrenar modelo final con todo Train
# 7. Generar submission
#
# No utiliza Holdout.
# No guarda modelos ni tablas.
# Única salida: CSV de Kaggle.
##########################################################


# 0. PAQUETES Y FUNCIONES ----------------------------------

library(xgboost)
library(caret)
library(dplyr)

source("01_code/functions/fun_metricas.R")
source("01_code/functions/fun_save_submission.R")


# 1. CARGAR DATOS ------------------------------------------

train <- readRDS("03_temp/train_clean.rds")
test <- readRDS("03_temp/test_clean.rds")

predictores <- readRDS("03_temp/predictores.rds")
cv_folds <- readRDS("03_temp/cv_folds.rds")


# 2. VALIDAR VARIABLES -------------------------------------

# Conjunto utilizado en nuestro XGBoost original.

nombre_conjunto <- "X_5_extendido"

if (!is.list(predictores) ||
    !nombre_conjunto %in% names(predictores)) {
  
  cat("\nConjuntos disponibles en predictores.rds:\n")
  print(names(predictores))
  
  stop(
    "No existe X_5_ampliado en predictores.rds. ",
    "Revisa si cambió el nombre del conjunto.",
    call. = FALSE
  )
}

variables_xgb <- predictores[[nombre_conjunto]]

if (!is.character(variables_xgb) ||
    length(variables_xgb) == 0 ||
    anyNA(variables_xgb) ||
    anyDuplicated(variables_xgb)) {
  
  stop(
    "X_5_ampliado debe contener un vector de ",
    "nombres de variables, sin NA ni duplicados.",
    call. = FALSE
  )
}

# Verificar que todas las variables existan en Train y Test.

faltantes_train <- setdiff(
  variables_xgb, names(train)
)

faltantes_test <- setdiff(
  variables_xgb, names(test)
)

if (length(faltantes_train) > 0 ||
    length(faltantes_test) > 0) {
  
  cat("\nVariables faltantes en Train:\n")
  print(faltantes_train)
  
  cat("\nVariables faltantes en Test:\n")
  print(faltantes_test)
  
  stop(
    "Hay variables faltantes en las bases. ",
    "Revisar preprocesamiento y predictores.rds.",
    call. = FALSE
  )
}

stopifnot(
  identical(levels(train$Pobre), c("Yes", "No")),
  "id" %in% names(test)
)

cat("\n=== DATOS ===\n")
cat("Hogares Train:", nrow(train), "\n")
cat("Hogares Test:", nrow(test), "\n")
cat("Predictores:", length(variables_xgb), "\n")


# 3. CONSTRUIR MATRICES ------------------------------------

# Utilizamos el mismo procedimiento del piloto original.
# Aquí ya sabemos que variables_xgb es válido y no está vacío.

matriz_train <- model.matrix(
  ~ .,
  data = train[, variables_xgb, drop = FALSE]
)[, -1, drop = FALSE]

matriz_test <- model.matrix(
  ~ .,
  data = test[, variables_xgb, drop = FALSE]
)[, -1, drop = FALSE]

y <- as.integer(train$Pobre == "Yes")

stopifnot(
  identical(
    colnames(matriz_train),
    colnames(matriz_test)
  ),
  nrow(matriz_train) == nrow(train),
  nrow(matriz_test) == nrow(test),
  !anyNA(matriz_train),
  !anyNA(matriz_test),
  all(is.finite(matriz_train)),
  all(is.finite(matriz_test))
)

cat("Columnas de matriz:", ncol(matriz_train), "\n")


# 4. VERIFICAR LOS CINCO FOLDS ------------------------------

stopifnot(length(cv_folds) == 5)

# Cada elemento debe contener los índices de entrenamiento.

stopifnot(all(vapply(
  cv_folds,
  function(idx) {
    is.numeric(idx) &&
      length(idx) > 0 &&
      !anyNA(idx) &&
      !anyDuplicated(idx) &&
      all(idx >= 1 & idx <= nrow(train))
  },
  logical(1)
)))

indices_validacion <- unlist(lapply(
  cv_folds,
  function(idx) {
    setdiff(seq_len(nrow(train)), idx)
  }
))

cat("\n=== VERIFICACIÓN DE FOLDS ===\n")
cat("Hogares Train:", nrow(train), "\n")
cat("Validaciones totales:", length(indices_validacion), "\n")
cat("Hogares únicos:", length(unique(indices_validacion)), "\n")

if (length(indices_validacion) != nrow(train) ||
    anyDuplicated(indices_validacion)) {
  
  stop(
    "Los folds NO corresponden a Train completo. ",
    "Vuelve a ejecutar 05_create_cv_folds.R.",
    call. = FALSE
  )
}

cat("Folds correctos.\n")


# 5. HIPERPARÁMETROS ---------------------------------------

# Exactamente la configuración original del piloto.

parametros <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  tree_method = "hist",
  max_depth = 5,
  eta = 0.10,
  min_child_weight = 5,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nthread = 8
)

nrounds_max <- 600
early_stopping <- 30


# 6. VALIDACIÓN CRUZADA ------------------------------------

prob_oof <- rep(NA_real_, nrow(train))

tiempos <- numeric(length(cv_folds))
mejores_rondas <- integer(length(cv_folds))

set.seed(2026)

inicio_total <- Sys.time()

for (k in seq_along(cv_folds)) {
  
  cat("\n================================\n")
  cat("XGBOOST - FOLD", k, "DE 5\n")
  cat("================================\n")
  
  inicio_fold <- Sys.time()
  
  
  # 6A. PARTICIÓN EXTERNA ---------------------------------
  
  idx_train <- cv_folds[[k]]
  
  idx_valid <- setdiff(
    seq_len(nrow(train)),
    idx_train
  )
  
  
  # 6B. PARTICIÓN INTERNA PARA EARLY STOPPING -------------
  
  idx_inner <- as.vector(
    caret::createDataPartition(
      train$Pobre[idx_train],
      p = 0.85,
      list = FALSE
    )
  )
  
  idx_ajuste <- idx_train[idx_inner]
  idx_parada <- idx_train[-idx_inner]
  
  
  # 6C. MATRICES XGBOOST ----------------------------------
  
  dtrain <- xgb.DMatrix(
    data = matriz_train[idx_ajuste, , drop = FALSE],
    label = y[idx_ajuste]
  )
  
  dparada <- xgb.DMatrix(
    data = matriz_train[idx_parada, , drop = FALSE],
    label = y[idx_parada]
  )
  
  dvalid <- xgb.DMatrix(
    data = matriz_train[idx_valid, , drop = FALSE]
  )
  
  
  # 6D. ENTRENAMIENTO -------------------------------------
  
  modelo <- xgb.train(
    params = parametros,
    data = dtrain,
    nrounds = nrounds_max,
    evals = list(validacion_interna = dparada),
    early_stopping_rounds = early_stopping,
    maximize = FALSE,
    verbose = 0
  )
  
  
  # 6E. MEJOR ITERACIÓN -----------------------------------
  
  mejor_ronda <- attr(modelo, "best_iteration")
  
  if (length(mejor_ronda) == 0) {
    mejor_ronda <- modelo$best_iteration
  }
  
  if (length(mejor_ronda) == 0) {
    
    valor <- xgboost::xgb.attr(
      modelo,
      "best_iteration"
    )
    
    if (length(valor) == 1) {
      mejor_ronda <- as.integer(valor) + 1L
    }
  }
  
  if (length(mejor_ronda) != 1 ||
      is.na(mejor_ronda)) {
    
    stop(
      "No se pudo recuperar la mejor ronda ",
      "del entrenamiento."
    )
  }
  
  mejores_rondas[k] <- as.integer(mejor_ronda)
  
  
  # 6F. PREDICCIONES OOF ----------------------------------
  
  prob_oof[idx_valid] <- as.numeric(
    predict(modelo, dvalid)
  )
  
  
  # 6G. TIEMPOS -------------------------------------------
  
  tiempos[k] <- as.numeric(
    difftime(
      Sys.time(),
      inicio_fold,
      units = "secs"
    )
  )
  
  cat("Mejor ronda:", mejores_rondas[k], "\n")
  cat("Tiempo:", round(tiempos[k], 1), "segundos\n")
  
  rm(modelo, dtrain, dparada, dvalid)
  
  gc(verbose = FALSE)
}


# 7. SELECCIÓN DEL UMBRAL -----------------------------------

stopifnot(
  !anyNA(prob_oof),
  all(is.finite(prob_oof)),
  all(prob_oof >= 0 & prob_oof <= 1)
)

umbrales <- seq(0.15, 0.50, by = 0.01)

resultados_f1 <- data.frame(
  Umbral = umbrales,
  F1 = vapply(
    umbrales,
    function(u) {
      
      fun_f1_score(
        .actual = train$Pobre,
        .predicted = ifelse(
          prob_oof >= u,
          "Yes",
          "No"
        ),
        .positive = "Yes"
      )
    },
    numeric(1)
  )
) %>%
  arrange(desc(F1))

mejor <- resultados_f1[1, , drop = FALSE]

umbral_final <- mejor$Umbral
f1_oof <- mejor$F1


# 8. RONDAS DEL MODELO DEFINITIVO ---------------------------

nrounds_final <- as.integer(
  round(mean(mejores_rondas))
)

cat("\n=== RONDAS SELECCIONADAS ===\n")
cat("Por fold:", mejores_rondas, "\n")
cat("Promedio:", nrounds_final, "\n")


# 9. ENTRENAMIENTO CON TODO TRAIN ---------------------------

dtrain_full <- xgb.DMatrix(
  data = matriz_train,
  label = y
)

dtest <- xgb.DMatrix(
  data = matriz_test
)

set.seed(2026)

modelo_final <- xgb.train(
  params = parametros,
  data = dtrain_full,
  nrounds = nrounds_final,
  verbose = 0
)


# 10. PREDICCIÓN SOBRE TEST ---------------------------------

prob_test <- as.numeric(
  predict(modelo_final, dtest)
)

stopifnot(
  length(prob_test) == nrow(test),
  all(is.finite(prob_test)),
  all(prob_test >= 0 & prob_test <= 1)
)


# 11. GENERAR SUBMISSION ------------------------------------

# Formato consistente con las convenciones del proyecto.

nombre_submission <- paste0(
  "XGB_depth_", parametros$max_depth,
  "_eta_010",
  "_mcw_", parametros$min_child_weight,
  "_rounds_", nrounds_final
)

submission_xgb <- fun_save_submission(
  .db_test = test,
  .prob = prob_test,
  .umbral = umbral_final,
  .nombre = nombre_submission
)


# 12. RESUMEN FINAL -----------------------------------------

tiempo_total <- as.numeric(
  difftime(
    Sys.time(),
    inicio_total,
    units = "mins"
  )
)

cat("\n========================================\n")
cat("           RESUMEN XGBOOST\n")
cat("========================================\n")

cat("\nDATOS:\n")
cat("Hogares Train:", nrow(train), "\n")
cat("Hogares Test:", nrow(test), "\n")
cat("Predictores:", length(variables_xgb), "\n")
cat("Columnas:", ncol(matriz_train), "\n")

cat("\nVALIDACIÓN CRUZADA:\n")
cat("Mejores rondas:", mejores_rondas, "\n")
cat("Rondas finales:", nrounds_final, "\n")
cat("Segundos por fold:", round(tiempos, 1), "\n")

cat("\nDESEMPEÑO:\n")
cat("Umbral óptimo:", umbral_final, "\n")
cat("F1 OOF:", round(f1_oof, 7), "\n")

cat("\nTOP 10 UMBRALES:\n")
print(head(resultados_f1, 10), digits = 7)

cat("\nSUBMISSION:\n")
cat("Pobres predichos:", sum(submission_xgb$pobre), "\n")
cat("Proporción pobre:",
    round(mean(submission_xgb$pobre), 4), "\n")

cat("\nArchivo generado:\n")
cat(
  paste0(
    "02_outputs/predictions/",
    nombre_submission,
    ".csv\n"
  )
)

cat("\nTiempo total:",
    round(tiempo_total, 2),
    "minutos\n")

cat("\nProceso terminado.\n")



############## CGBOOST EXHAUSTIVO