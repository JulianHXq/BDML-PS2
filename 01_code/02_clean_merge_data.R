##########################################################
# 02_clean_merge_data.R
#
# Objetivo: Limpiar las bases de personas y hogares, construir variables a nivel
# hogar a partir de las personas y unir ambas bases.
#
# Etapa del Problem Set: Etapa 1: Understanding the Data.
# Insumos:  00_data/raw/ (train/test de hogares y personas).
# Salidas:  03_temp/train_clean.rds y 03_temp/test_clean.rds.
##########################################################

# Paquetes y funciones -----------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, scales, rio, tidymodels, ggplot2,dplyr,tidyr,tinytex)

message("Necessary packages succesfully loaded")

source("01_code/functions/fun_preprocess_personas.R")
source("01_code/functions/fun_colapse_personas.R")
source("01_code/functions/fun_preprocess_hogares.R")

# Lectura de los datos crudos ----------------------------------------------
db_train_hogares  <- read.csv("00_data/raw/train_hogares.csv")
db_train_personas <- read.csv("00_data/raw/train_personas.csv")
db_test_hogares   <- read.csv("00_data/raw/test_hogares.csv")
db_test_personas  <- read.csv("00_data/raw/test_personas.csv")

# Personas: limpieza y colapso a nivel hogar -------------------------------
db_train_personas_hogar <- db_train_personas |>
  fun_preprocess_personas() |>
  fun_colapse_personas()

db_test_personas_hogar <- db_test_personas |>
  fun_preprocess_personas() |>
  fun_colapse_personas()

# Hogares: preparación -----------------------------------------------------
db_train_hogares <- fun_preprocess_hogares(db_train_hogares)
db_test_hogares  <- fun_preprocess_hogares(db_test_hogares)

# Unión de hogares y personas ----------------------------------------------
# La base de hogares define las filas: una por hogar.
db_train <- db_train_hogares |>
  left_join(db_train_personas_hogar, by = "id")

db_test <- db_test_hogares |>
  left_join(db_test_personas_hogar, by = "id")

# Verificaciones -----------------------------------------------------------
# Antes de guardar, se confirma que la unión no perdió, duplicó ni dejó sin
# información a ningún hogar. Si algo falla, el script se detiene aquí.
fun_verificar_base <- function(.db, .db_hogares, .nombre) {
  if (nrow(.db) != nrow(.db_hogares)) {
    stop(.nombre, ": la unión cambió el número de hogares (",
         nrow(.db_hogares), " antes, ", nrow(.db), " después).", call. = FALSE)
  }
  if (anyDuplicated(.db$id) > 0) {
    stop(.nombre, ": hay hogares repetidos.", call. = FALSE)
  }
  if (anyNA(.db)) {
    columnas_na <- names(.db)[colSums(is.na(.db)) > 0]
    stop(.nombre, ": hay valores faltantes en ",
         paste(columnas_na, collapse = ", "), ".", call. = FALSE)
  }
  message(.nombre, ": ", nrow(.db), " hogares y ", ncol(.db),
          " columnas, sin faltantes ni duplicados.")
}

fun_verificar_base(db_train, db_train_hogares, "Train")
fun_verificar_base(db_test, db_test_hogares, "Test")

# Train y test deben tener los mismos predictores; train además trae Pobre.
if (!setequal(setdiff(names(db_train), "Pobre"), names(db_test))) {
  stop("Train y test no tienen las mismas columnas.", call. = FALSE)
}

# Id -----------------------------------------------------------------------
# En train, el id no es un predictor: se elimina para que ningún modelo lo
# use por error. En test se conserva, porque es la llave del archivo que se 
# sube a Kaggle. Se elimina después de verificar, porque la revisión de hogares 
# repetidos lo necesita.
db_train <- db_train |>
  select(-id) |>
  relocate(Pobre)

# Guardar ------------------------------------------------------------------
dir.create("03_temp", showWarnings = FALSE)
saveRDS(db_train, "03_temp/train_clean.rds")
saveRDS(db_test, "03_temp/test_clean.rds")
message("Bases guardadas en 03_temp/train_clean.rds y 03_temp/test_clean.rds.")


# Se liberan los objetos de este script: las bases de personas ocupan cientos
# de MB y los scripts siguientes leen las bases limpias desde 03_temp/.
rm(db_train_hogares, db_train_personas, db_test_hogares, db_test_personas,
   db_train_personas_hogar, db_test_personas_hogar, db_train, db_test,
   fun_verificar_base)
