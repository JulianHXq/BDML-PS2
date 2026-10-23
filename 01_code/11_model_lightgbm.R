##########################################################
# 11_model_lightgbm.R
# LightGBM individual con 1.296 predictores reconstruidos desde los CSV.
# Ejecutar desde la raiz de ESTE repositorio (ContinuarExperrimento):
#   source("01_code/11_model_lightgbm.R")
# O desde terminal:
#   Rscript 01_code/11_model_lightgbm.R
#
# Usa las funciones existentes, ampliadas al final de sus mismos archivos.
# El argumento .ampliado=TRUE activa esta ingenieria; las llamadas anteriores
# conservan exactamente su comportamiento. No requiere correr 00_rundirectory.R
# ni modificar los archivos train_clean.rds, test_clean.rds o los folds previos.
# Lee exclusivamente los cuatro CSV locales de 00_data/raw/ (o 00_data/).
# Genera predicciones 1=pobre, 0=no pobre en 02_outputs/predictions/.
# Conserva salidas anteriores: cada repeticion usa nombres nuevos.
#
# Parametros fijados por el desarrollo original: LightGBM 4.7.0, 748 rondas,
# umbral 0.375. F1 OOF historico: 0.7681869862630706, clase positiva Yes=1.
# Reproduce el ajuste final; no reestima ese F1 ni selecciona usando reserva.
# Bogota participa en el ajuste final como informacion auxiliar. El F1
# historico se obtuvo exclusivamente con validacion fuera de Bogota.
# Opcion tecnica: --auditar-columnas genera huellas de todas las columnas.
##########################################################


local({
  inicio <- proc.time()[["elapsed"]]
  if (!dir.exists("00_data") || !dir.exists("01_code/functions")) {
    stop("Ejecute desde la raiz de ContinuarExperrimento o abra BDML-PS2.Rproj.")
  }
  lib <- file.path("03_temp", "r_library")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(normalizePath(lib, winslash = "/"), .libPaths()))
  auditoria_columnas <- "--auditar-columnas" %in% commandArgs(trailingOnly = TRUE)
  paquetes <- c("R6", "jsonlite", "data.table", "Matrix", "lightgbm")
  if (auditoria_columnas) paquetes <- c(paquetes, "digest")
  faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan)) {
    descargas <- file.path("03_temp", "r_downloads")
    dir.create(descargas, recursive = TRUE, showWarnings = FALSE)
    options(timeout = max(300L, getOption("timeout")))
    install.packages(faltan, lib = lib, destdir = descargas,
                     repos = "https://cloud.r-project.org")
  }
  if (!all(vapply(paquetes, requireNamespace, logical(1), quietly = TRUE))) {
    stop("Faltan dependencias. Instale los paquetes indicados desde CRAN.")
  }
  if (as.character(utils::packageVersion("lightgbm")) != "4.7.0") {
    stop("Se requiere LightGBM 4.7.0 para reproducir esta especificacion.")
  }
  suppressPackageStartupMessages(library(data.table))
  hilos <- min(6L, parallel::detectCores(logical = TRUE))
  if (is.na(hilos) || hilos < 1L) hilos <- 1L
  data.table::setDTthreads(hilos)
  semilla <- 20261022L
  set.seed(semilla)
  umbral <- 0.3750000000000002
  rondas <- 748L
  funciones <- c("fun_preprocess_hogares.R", "fun_preprocess_personas.R",
                 "fun_conteos_personas.R", "fun_colapse_personas.R", "fun_save_submission.R")
  for (archivo in funciones) sys.source(file.path("01_code/functions", archivo), envir = environment())

  # Todas las escrituras de esta ejecucion quedan en rutas nuevas.
  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  carpeta_modelo <- file.path("03_temp", paste0("lightgbm_ampliado_", run_id))
  replica <- 1L
  while (dir.exists(carpeta_modelo)) {
    replica <- replica + 1L
    carpeta_modelo <- file.path("03_temp", paste0("lightgbm_ampliado_", run_id, "_", replica))
  }
  dir.create(carpeta_modelo, recursive = TRUE)
  dir.create("02_outputs/predictions", recursive = TRUE, showWarnings = FALSE)
  dir.create("02_outputs/tables", recursive = TRUE, showWarnings = FALSE)
  archivos <- c("train_hogares.csv", "test_hogares.csv", "train_personas.csv", "test_personas.csv")
  datos <- "00_data/raw"
  if (!all(file.exists(file.path(datos, archivos)))) datos <- "00_data"
  rutas <- file.path(datos, archivos)
  if (!all(file.exists(rutas))) stop("Faltan CSV locales en 00_data. Se requieren los cuatro originales.")
  huellas_antes <- tools::md5sum(rutas)
  cabeceras <- lapply(rutas, function(r) names(fread(r, nrows = 0L, showProgress = FALSE)))
  comunes_hh <- intersect(cabeceras[[1L]], cabeceras[[2L]])
  comunes_pp <- intersect(cabeceras[[3L]], cabeceras[[4L]])
  leer <- function(ruta, columnas) {
    fread(ruta, select = columnas,
          colClasses = list(character = intersect(c("id", "Depto", "Clase"), columnas)),
          showProgress = FALSE)
  }
  matrices <- list()
  metadatos <- list()
  for (tipo in c("train", "test")) {
    i_hh <- if (tipo == "train") 1L else 2L
    i_pp <- if (tipo == "train") 3L else 4L
    h <- leer(rutas[i_hh], cabeceras[[i_hh]][cabeceras[[i_hh]] %in% c(comunes_hh, "Pobre")])
    p <- leer(rutas[i_pp], cabeceras[[i_pp]][cabeceras[[i_pp]] %in% comunes_pp])
    message("Ingenieria ", tipo, ": ", nrow(h), " hogares; ", nrow(p), " personas.")
    h <- fun_preprocess_hogares(h, .ampliado = TRUE, .columnas_comunes = comunes_hh)
    p <- fun_preprocess_personas(p, .ampliado = TRUE, .columnas_comunes = comunes_pp)
    matriz <- fun_colapse_personas(p, .hogares = h, .ampliado = TRUE)
    metadatos[[tipo]] <- data.table(id = h$id, Dominio = h$Dominio)
    if (tipo == "train") metadatos[[tipo]][, Pobre := as.integer(h$Pobre)]
    columnas <- attr(matriz, "lgbm_feature_columns")
    matrices[[tipo]] <- matriz[, columnas, with = FALSE]
    message("  ", tipo, ": ", length(columnas), " columnas antes de excluir factores de expansion.")
    rm(h, p, matriz)
    gc(verbose = FALSE)
  }
  stopifnot(!any(metadatos$train$id %in% metadatos$test$id))
  x <- matrices$train
  z <- matrices$test
  for (c in setdiff(names(x), names(z))) {
    data.table::set(z, j = c, value = rep(if (startsWith(c, "category_")) 0 else NA_real_, nrow(z)))
  }
  z <- z[, names(x), with = FALSE]
  categorias <- c("hh_Dominio", "hh_Depto", "hh_Clase")
  niveles_guardados <- list()
  for (c in categorias) {
    niveles <- sort(unique(c(as.character(x[[c]]), as.character(z[[c]]))), method = "radix")
    stopifnot(!anyNA(niveles))
    niveles_guardados[[c]] <- niveles
    data.table::set(x, j = c, value = match(as.character(x[[c]]), niveles) - 1L)
    data.table::set(z, j = c, value = match(as.character(z[[c]]), niveles) - 1L)
  }
  columnas <- names(x)[!startsWith(names(x), "survey_")]
  stopifnot(length(columnas) == 1296L, !anyDuplicated(columnas))
  x <- as.matrix(x[, columnas, with = FALSE])
  z <- as.matrix(z[, columnas, with = FALSE])
  storage.mode(x) <- storage.mode(z) <- "double"
  stopifnot(identical(colnames(x), colnames(z)), !any(is.infinite(x)), !any(is.infinite(z)))
  rm(matrices)
  gc(verbose = FALSE)

  if (auditoria_columnas) {
    for (tipo in c("train", "test")) {
      mat <- if (tipo == "train") x else z
      huellas <- vapply(seq_along(columnas), function(j) {
        v <- mat[, j]
        faltante <- is.na(v)
        v[faltante] <- 0
        digest::digest(c(writeBin(v, raw(), size = 4L, endian = "little"), as.raw(faltante)),
                       algo = "sha256", serialize = FALSE)
      }, character(1))
      fwrite(data.table(feature = columnas, sha256_values_and_missing = huellas),
             file.path(carpeta_modelo, paste0("auditoria_columnas_", tipo, ".csv")))
    }
    rm(mat)
  }
  orden <- c(which(metadatos$train$Dominio != "BOGOTA"), which(metadatos$train$Dominio == "BOGOTA"))
  stopifnot(length(orden) == nrow(x), !anyDuplicated(orden))
  y <- metadatos$train$Pobre[orden]
  stopifnot(all(y %in% c(0L, 1L)), all(c(0L, 1L) %in% y))
  x <- x[orden, , drop = FALSE]
  parametros <- list(objective = "binary", metric = "binary_logloss", boosting = "gbdt",
    learning_rate = 0.04, num_leaves = 31L, max_depth = -1L,
    min_data_in_leaf = 100L, min_sum_hessian_in_leaf = 0.001,
    feature_fraction = 0.85, bagging_fraction = 0.85, bagging_freq = 1L,
    lambda_l1 = 0.1, lambda_l2 = 10, min_gain_to_split = 0,
    max_bin = 127L, bin_construct_sample_cnt = 200000L,
    seed = semilla, deterministic = TRUE, force_col_wise = TRUE, num_threads = hilos, verbosity = -1L)
  dataset <- lightgbm::lgb.Dataset(x, label = y, categorical_feature = categorias,
                                  params = parametros, free_raw_data = TRUE)
  message("Ajuste individual LightGBM: ", nrow(x), " hogares; ", ncol(x), " predictores; ", rondas, " rondas.")
  modelo <- lightgbm::lgb.train(params = parametros, data = dataset, nrounds = rondas, verbose = -1L)
  rm(x, dataset)
  gc(verbose = FALSE)
  prob <- as.numeric(predict(modelo, z, params = list(num_threads = hilos)))
  stopifnot(length(prob) == nrow(metadatos$test), all(is.finite(prob)), all(prob >= 0 & prob <= 1))
  nombre <- "LGBM_hojas_31_lambda_10_alpha_01_iter_748_umbral_0375"
  base_nombre <- nombre
  replica <- 1L
  while (any(file.exists(file.path("02_outputs/predictions", paste0(nombre, c(".csv", "_con_scores.csv")))))) {
    nombre <- paste0(base_nombre, "_replica_", run_id, "_", replica)
    replica <- replica + 1L
  }
  submission <- fun_save_submission(.db_test = metadatos$test, .prob = prob, .umbral = umbral,
    .nombre = nombre, .sample = file.path(datos, "sample_submission.csv"), .proteger = TRUE)
  stopifnot(identical(submission$id, metadatos$test$id), all(submission$pobre %in% c(0L, 1L)))
  fwrite(data.table(id = submission$id, pobre = submission$pobre, score_Pobre_1 = prob),
         file.path("02_outputs/predictions", paste0(nombre, "_con_scores.csv")))
  lightgbm::lgb.save(modelo, file.path(carpeta_modelo, "modelo_final.txt"))
  fwrite(data.table(feature = columnas), file.path(carpeta_modelo, "variables.csv"))
  jsonlite::write_json(niveles_guardados, file.path(carpeta_modelo, "categorias.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  huellas_despues <- tools::md5sum(rutas)
  stopifnot(identical(huellas_antes, huellas_despues))
  resumen <- data.frame(archivo = paste0(nombre, ".csv"), algoritmo = "LightGBM",
    predictores = length(columnas), rondas = rondas, umbral = umbral,
    clase_positiva = 1L, alias_positivo = "Yes", train_n = nrow(metadatos$train),
    train_bogota_n = sum(metadatos$train$Dominio == "BOGOTA"), test_n = nrow(submission),
    predichos_pobres = sum(submission$pobre), F1_OOF_historico = 0.7681869862630706,
    F1_reestimado_en_esta_ejecucion = FALSE, segundos = proc.time()[["elapsed"]] - inicio)
  tabla <- file.path("02_outputs/tables", paste0(basename(carpeta_modelo), "_resumen.csv"))
  stopifnot(!file.exists(tabla))
  fwrite(resumen, tabla)
  jsonlite::write_json(list(parameters = parametros, summary = resumen,
    metric_scope = "F1 historico de desarrollo usado en seleccion; no es evaluacion independiente ni F1 de test.",
    original_md5_before = as.list(huellas_antes), original_md5_after = as.list(huellas_despues),
    source_functions = funciones, expanded_mode = TRUE),
    file.path(carpeta_modelo, "ejecucion.json"), pretty = TRUE, auto_unbox = TRUE, digits = 17)
  writeLines(capture.output(sessionInfo()), file.path(carpeta_modelo, "sessionInfo.txt"))
  message("LightGBM completado: ", file.path("02_outputs/predictions", paste0(nombre, ".csv")))
  message("Originales intactos. Tiempo total: ", round(proc.time()[["elapsed"]] - inicio, 1), " segundos.")
  invisible(submission)
})
