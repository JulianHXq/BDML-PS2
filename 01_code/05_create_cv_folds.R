##########################################################
# 05_create_cv_folds.R
#
# Objetivo: Crear una sola vez las particiones de validación cruzada (k = 5,
# semilla fija) para que todos los modelos se evalúen sobre los mismos
# hogares.
#
# Etapa del Problem Set: Base de la Etapa 2.
# Insumos:  03_temp/train_clean.rds.
# Salidas:  03_temp/cv_folds.rds.
##########################################################

# Paquetes ------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, caret)

train <- readRDS("03_temp/train_clean.rds")

# Particiones --------------------------------------------------------------
# createFolds() reparte los hogares de forma estratificada según Pobre: cada
# fold queda con cerca del 20% de hogares pobres, igual que la muestra
# completa. Con returnTrain = TRUE, cada elemento contiene los hogares con
# los que se entrena en ese fold, que es lo que espera el argumento index de
# trainControl(). Al guardar las particiones, todos los modelos se validan
# con exactamente los mismos hogares, y la diferencia de desempeño entre ellos
# no se debe a cómo se partieron los datos.
set.seed(2026)
cv_folds <- createFolds(train$Pobre, k = 5, returnTrain = TRUE)

# Verificaciones -----------------------------------------------------------
# Cada hogar debe quedar en el grupo de validación de exactamente un fold.
hogares_validacion <- unlist(lapply(cv_folds, function(idx) {
  setdiff(seq_len(nrow(train)), idx)
}))
if (!setequal(hogares_validacion, seq_len(nrow(train))) ||
    anyDuplicated(hogares_validacion) > 0) {
  stop("Las particiones no cubren cada hogar exactamente una vez.",
       call. = FALSE)
}

# Proporción de hogares pobres en el grupo de validación de cada fold.
prop_pobres <- sapply(cv_folds, function(idx) mean(train$Pobre[-idx] == "Yes"))
message("Proporción de pobres por fold de validación: ",
        paste(sprintf("%.3f", prop_pobres), collapse = ", "), ".")

# Guardar ------------------------------------------------------------------
saveRDS(cv_folds, "03_temp/cv_folds.rds")

rm(train, cv_folds, hogares_validacion, prop_pobres)


