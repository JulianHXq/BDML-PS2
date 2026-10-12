##########################################################
# fun_colapse_personas.R
#
# Función para llevar la base de personas a nivel hogar: construye variables
# del hogar a partir de sus miembros. Adaptada de la Clase 7, con una
# corrección en la tasa de dependencia. Requiere que el paquete tidyverse
# esté cargado.
##########################################################

fun_colapse_personas <- function(.db) {
  #' Colapsar a nivel hogar las observaciones de personas
  #'
  #' @description \code{fun_colapse_personas()} retorna una base de datos con
  #' variables a nivel de hogar a partir de observaciones de personas.
  #'
  #' @param .db \code{tibble}. Base de datos de personas preprocesada
  #' previamente por la función \code{fun_preprocess_personas()}.
  #'
  #' @returns \code{tibble}. Una fila por hogar con el id del hogar, \code{id};
  #' si el jefe del hogar es hombre, \code{bin_head_male}; el número de menores
  #' de 18, \code{num_minors}, de mayores de 65, \code{num_old}, de inactivos
  #' en edad de trabajar, \code{num_inactivos}, y de personas en edad de
  #' trabajar, \code{num_edad_trabajar}; la tasa de dependencia,
  #' \code{prop_dependencia}; la tasa de inactividad,
  #' \code{prop_inactividad}; y el máximo nivel educativo del hogar,
  #' \code{cat_maxEduc}.
  
  db_result <- .db |>
    group_by(id) |>
    summarize(bin_head_male = max(bin_head * bin_male, na.rm = TRUE),
              num_minors = sum(bin_minor, na.rm = TRUE),
              num_old = sum(bin_old, na.rm = TRUE),
              num_inactivos = sum(bin_inactivo * bin_edad_trabajar, na.rm = TRUE),
              num_edad_trabajar = sum(bin_edad_trabajar, na.rm = TRUE),
              cat_maxEduc = max(cat_educ, na.rm = TRUE)) |>
    mutate(
      # Tasa de dependencia: dependientes (menores de 18 y mayores de 65) por
      # cada persona en edad de trabajar (18 a 65).
      # CORRECCIÓN frente a la Clase 7: allí el denominador era el número de
      # personas fuera de la edad de trabajar, que son exactamente los mismos
      # dependientes del numerador. La "tasa" solo podía valer 0 o 1 y
      # funcionaba como un indicador de "hay dependientes".
      prop_dependencia = (num_minors + num_old) / na_if(num_edad_trabajar, 0),
      prop_inactividad = num_inactivos / na_if(num_edad_trabajar, 0),
      # Sin miembros de 18 a 65 años (sobre todo adultos mayores que viven
      # solos, 6,4% de los hogares de entrenamiento), ambas tasas no están
      # definidas. Como en la Clase 7, se reemplazan por 0. Estos hogares
      # siguen siendo identificables porque tienen num_edad_trabajar = 0.
      prop_dependencia = replace_na(prop_dependencia, 0),
      prop_inactividad = replace_na(prop_inactividad, 0)
    ) |>
    select(id, bin_head_male, num_minors, num_old, num_inactivos,
           num_edad_trabajar, prop_dependencia, prop_inactividad,
           cat_maxEduc) |>
    ungroup()
  
  return(db_result)
}