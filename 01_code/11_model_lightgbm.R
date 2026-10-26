##########################################################
# 11_model_lightgbm.R
#
# Objetivo: Validar con validación cruzada el modelo de boosting LightGBM con
# el conjunto ampliado X_7 (1.296 predictores) y generar su predicción para
# Kaggle.
#
# Etapa del Problem Set: Etapa 2: Training and Comparing Algorithms.
# Insumos:  03_temp/ (train_X7.rds, test_X7.rds, train_clean.rds,
#           predictores.rds y cv_folds.rds).
# Salidas:  02_outputs/predictions/ (el envío a Kaggle), una fila en
#           02_outputs/tables/registro_modelos.csv y los resultados en
#           03_temp/.
##########################################################
#
# Este script se puede correr solo, siempre que antes se hayan corrido los
# scripts 01 a 05. Necesita unos 7 GB de memoria RAM.
#
# Estructura (la misma de los demás modelos):
#   1. Validación cruzada con las particiones de 03_temp/cv_folds.rds.
#   2. Elegir el umbral que maximiza el F1 con las predicciones fuera de
#      muestra.
#   3. Reentrenar con todo train, predecir el test y guardar el envío.
##########################################################

# Preparación -------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, caret, pROC, lightgbm)

source("01_code/functions/fun_metricas.R")
source("01_code/functions/fun_save_submission.R")
source("01_code/functions/fun_variables_lgbm.R")

predictores <- readRDS("03_temp/predictores.rds")
cv_folds    <- readRDS("03_temp/cv_folds.rds")
train_clean <- readRDS("03_temp/train_clean.rds")

# Resultado, de train_clean (mismo orden de hogares que train_X7,
# verificado en 02_clean_merge_data.R).
y <- as.integer(train_clean$Pobre == "Yes")
rm(train_clean)

# Matriz de train. Las particiones guardan posiciones de fila: train_X7 debe
# tener exactamente los hogares de train_clean, en el mismo orden.
columnas <- predictores$X_7_ampliado
base_train <- readRDS("03_temp/train_X7.rds")
stopifnot(nrow(base_train) == length(y),
          max(unlist(cv_folds)) == length(y),
          length(unique(unlist(cv_folds))) == length(y))
x <- fun_matriz_x7(base_train, columnas)
rm(base_train); gc()

# Categóricas de ubicación: vienen como códigos enteros (02); LightGBM las
# trata como categorías y no como números ordenados.
categorias <- c("hh_Dominio", "hh_Depto", "hh_Clase")

# Hiperparámetros --------------------------------------------------------------
# Árboles de hasta 31 hojas, tasa de aprendizaje de 0,04 y 748 rondas;
# penalidad L2 (lambda_l2 = 10) y L1
# (lambda_l1 = 0,1) sobre los valores de las hojas; al menos 100 hogares
# por hoja; y en cada ronda, 85% de las variables y 85% de los hogares,
# para reducir la varianza. La semilla y deterministic = TRUE hacen que el
# resultado se repita en cada corrida.
semilla <- 20261022L
parametros <- list(objective = "binary", metric = "binary_logloss",
                   boosting = "gbdt", learning_rate = 0.04, num_leaves = 31L,
                   max_depth = -1L, min_data_in_leaf = 100L,
                   min_sum_hessian_in_leaf = 0.001, feature_fraction = 0.85,
                   bagging_fraction = 0.85, bagging_freq = 1L,
                   lambda_l1 = 0.1, lambda_l2 = 10, min_gain_to_split = 0,
                   max_bin = 127L, bin_construct_sample_cnt = 200000L,
                   seed = semilla, deterministic = TRUE,
                   force_col_wise = TRUE,
                   num_threads = max(1L, min(6L, parallel::detectCores())),
                   verbosity = -1L)
rondas <- 748L

# 1. Validación cruzada ---------------------------------------------------------
# En cada fold se entrena con las filas de cv_folds[[k]] y se predicen las
# demás. Así el F1 es comparable con el de los demás modelos, validados
# con los mismos hogares.
prob_oof <- rep(NA_real_, nrow(x))
for (k in seq_along(cv_folds)) {
  inicio <- Sys.time()
  idx_train <- cv_folds[[k]]
  idx_valid <- setdiff(seq_len(nrow(x)), idx_train)
  
  datos_fold <- lgb.Dataset(x[idx_train, , drop = FALSE], label = y[idx_train],
                            categorical_feature = categorias,
                            params = parametros, free_raw_data = TRUE)
  modelo_fold <- lgb.train(params = parametros, data = datos_fold,
                           nrounds = rondas, verbose = -1L)
  prob_oof[idx_valid] <- as.numeric(predict(modelo_fold,
                                            x[idx_valid, , drop = FALSE]))
  
  message("Fold ", k, " listo en ",
          round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1),
          " minutos.")
  rm(datos_fold, modelo_fold); gc(verbose = FALSE)
}
stopifnot(!anyNA(prob_oof))

# 2. Umbral --------------------------------------------------------------------------
# Umbral que maximiza el F1 con las predicciones fuera de muestra, como en
# los demás modelos. Evidencia: umbral de 0,5 frente al umbral elegido, con
# el número de falsos positivos (leakage) y falsos negativos
# (undercoverage).
obs <- factor(ifelse(y == 1, "Yes", "No"), levels = c("Yes", "No"))
res_umbral <- fun_umbral_f1(obs, prob_oof)

comparacion <- bind_rows(
  fun_metricas_umbral(obs, prob_oof, 0.5) |>
    mutate(estrategia = "Umbral 0,5"),
  fun_metricas_umbral(obs, prob_oof, res_umbral$umbral) |>
    mutate(estrategia = "Umbral que maximiza F1")
) |>
  relocate(estrategia)
print(comparacion)

# 3. Modelo final y envío ----------------------------------------------------------
# Se reentrena con todo train, con la misma configuración.
datos_final <- lgb.Dataset(x, label = y,
                           categorical_feature = categorias,
                           params = parametros, free_raw_data = TRUE)
rm(x); gc()
modelo <- lgb.train(params = parametros, data = datos_final, nrounds = rondas,
                    verbose = -1L)
rm(datos_final); gc()

base_test <- readRDS("03_temp/test_X7.rds")
id_test <- base_test$id
z <- fun_matriz_x7(base_test, columnas)
rm(base_test); gc()
prob_test <- as.numeric(predict(modelo, z))
rm(z); gc()

# Envío con el umbral que maximiza el F1 en la validación cruzada. El nombre
# indica el algoritmo, el conjunto, los hiperparámetros principales y el
# umbral.
umbral <- res_umbral$umbral
archivo <- paste0("LGBM_X_7_ampliado_hojas_31_iter_", rondas, "_umbral_",
                  fun_hiperparam_str(umbral, .digits = 2))
fun_save_submission(data.frame(id = id_test), prob_test, umbral, archivo)

# Registro --------------------------------------------------------------------------
# El puntaje público de Kaggle se anota a mano en 00_data/kaggle_scores.csv,
# con el mismo nombre de archivo.
metricas_cv <- comparacion |> filter(estrategia == "Umbral que maximiza F1")
fun_registrar_modelo(tibble(
  archivo = archivo,
  algoritmo = "Boosting (LightGBM)",
  conjunto = "X_7_ampliado",
  n_predictores = length(columnas),
  hiperparametros = paste0("hojas = 31; learning_rate = 0.04; ",
                           "lambda_l2 = 10; lambda_l1 = 0.1; rondas = ", rondas),
  umbral = umbral,
  f1_cv = metricas_cv$F1,
  precision_cv = metricas_cv$precision,
  recall_cv = metricas_cv$recall,
  auc_cv = res_umbral$auc,
  fecha = as.character(Sys.Date())
))

# Se guardan las predicciones fuera de muestra, las del test y la
# importancia de las variables (ganancia), para la comparación de algoritmos
# y el análisis del mejor modelo.
saveRDS(list(oof = data.frame(rowIndex = seq_along(prob_oof), obs = obs,
                              Yes = prob_oof),
             prob_test = prob_test, umbral = umbral, comparacion = comparacion,
             importancia = lgb.importance(modelo)),
        file.path("03_temp", paste0(archivo, ".rds")))

cat("\nUmbral que maximiza F1 en CV:", round(umbral, 3),
    "- F1:", round(metricas_cv$F1, 4), "\n")
