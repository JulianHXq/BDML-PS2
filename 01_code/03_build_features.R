##########################################################
# 03_build_features.R
#
# Objetivo: Definir, con un nombre cada uno, los conjuntos de predictores
# (listas de variables) entre los que escogen los scripts de modelos.
#
# - Envíos a Kaggle: cada variante puede usar el conjunto que quiera. Probar
#   distintos conjuntos es parte de la iteración que pide la Etapa 2.
# - Comparación de algoritmos (deck compare): solo se comparan envíos que
#   usaron el MISMO conjunto, para que la diferencia de puntaje refleje el
#   algoritmo y no las variables. Por eso cada algoritmo debe tener al menos
#   una variante con el conjunto común.
#
# Etapa del Problem Set: Etapa 1 y base de la Etapa 2.
# Insumos:  03_temp/train_clean.rds y 03_temp/test_clean.rds.
# Salidas:  03_temp/predictores.rds.
##########################################################

# Paquetes ------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

train <- readRDS("03_temp/train_clean.rds")
test  <- readRDS("03_temp/test_clean.rds")

# Conjuntos anidados por bloques ------------------------------------------
# Cada conjunto contiene al anterior y le agrega un bloque de variables con
# una razón económica común. Los bloques van de lo más barato y fácil de
# verificar en una visita al hogar (ubicación, vivienda, tamaño) a lo más
# costoso, que depende de lo que el hogar declara (horas, cotización). Así se
# puede medir cuánto se pierde en predicción con una encuesta más corta.

# Bloque 1. Ubicación, vivienda y tamaño del hogar: observables en una visita
# corta.
X_1_vivienda <- c("cat_ubicacion", "cat_tenencia", "num_personas_cuarto",
                  "num_personas_ug", "num_minors")

# Bloque 2. Composición del hogar y capital humano: cuántos dependen de
# quienes pueden trabajar, y qué educación tienen.
X_2_demografia <- c(X_1_vivienda,
                    "bin_head_male", "num_old", "num_edad_trabajar",
                    "prop_dependencia", "cat_maxEduc",
                    "num_educ_jefe_conyuge")

# Bloque 3. Empleo: cuántos miembros trabajan y cuántos están inactivos.
X_3_empleo <- c(X_2_demografia,
                "num_ocupados", "prop_ocupados", "num_inactivos",
                "prop_inactividad")

# Bloque 4. Calidad del empleo y formalidad: cuánto y cómo se trabaja, y si
# hay cotización o pensión. Con este bloque están los 21 predictores.
X_4_completo <- c(X_3_empleo,
                  "num_cotizantes", "bin_hogar_pensionado", "num_horas_ug",
                  "num_asalariados", "num_cuentapropia", "cat_pos_jefe")

# Bloque 5. Conjunto extendido: X_4_completo más todas las variables
# agregadas después (arriendo, demografía detallada, desempleo, estudiantes,
# afiliación a salud, subempleo, formalidad y transferencias). Se define
# como todas las columnas de la base para que incluya automáticamente
# cualquier variable nueva de la limpieza.
X_5_extendido <- union(X_4_completo,
                       setdiff(names(train), c("id", "Pobre")))


predictores <- list(X_1_vivienda   = X_1_vivienda,
                    X_2_demografia = X_2_demografia,
                    X_3_empleo     = X_3_empleo,
                    X_4_completo   = X_4_completo,
                    X_5_extendido  = X_5_extendido)

# Verificaciones -----------------------------------------------------------
# El conjunto completo debe tener exactamente los predictores de las bases:
# si se agrega o quita una variable en la limpieza y no se actualiza aquí,
# el script se detiene en lugar de dejar un conjunto incompleto.
if (!setequal(X_5_extendido, setdiff(names(train), c("id", "Pobre"))) ||
    !setequal(X_5_extendido, setdiff(names(test), "id"))) {
  stop("X_5_extendido no coincide con los predictores de train y test.",
       call. = FALSE)
}
# Ningún conjunto debe repetir variables.
if (any(sapply(predictores, anyDuplicated) > 0)) {
  stop("Hay variables repetidas en algún conjunto.", call. = FALSE)
}

message("Conjuntos de predictores: ",
        paste0(names(predictores), " (", lengths(predictores), ")",
               collapse = ", "), ".")

# Guardar ------------------------------------------------------------------
saveRDS(predictores, "03_temp/predictores.rds")

rm(train, test, X_1_vivienda, X_2_demografia, X_3_empleo, X_4_completo, X_5_extendido,
   predictores)
