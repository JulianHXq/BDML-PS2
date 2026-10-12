##########################################################
# fun_colapse_personas.R
#
# Función para llevar la base de personas a nivel hogar: construye variables
# del hogar a partir de sus miembros. Requiere que el paquete tidyverse esté
# cargado.
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
  #' \code{prop_inactividad}; el máximo nivel educativo del hogar,
  #' \code{cat_maxEduc}; y, solo para la unidad de gasto, el número de
  #' personas, \code{num_personas_ug}, de ocupados, \code{num_ocupados}, y de
  #' cotizantes a pensión, \code{num_cotizantes}; la proporción de ocupados,
  #' \code{prop_ocupados}; y si alguien recibe pensión,
  #' \code{bin_hogar_pensionado}.
  
  db_result <- .db |>
    group_by(id) |>
    summarize(bin_head_male = max(bin_head * bin_male, na.rm = TRUE),
              num_minors = sum(bin_minor, na.rm = TRUE),
              num_old = sum(bin_old, na.rm = TRUE),
              num_inactivos = sum(bin_inactivo * bin_edad_trabajar, na.rm = TRUE),
              num_edad_trabajar = sum(bin_edad_trabajar, na.rm = TRUE),
              cat_maxEduc = max(cat_educ, na.rm = TRUE),
              # Variables de la unidad de gasto: se multiplica por bin_ug para
              # contar solo a quienes comparten el presupuesto del hogar
              # (excluye servicio doméstico, pensionistas y trabajadores).
              # num_personas_ug coincide con Npersug de la base de hogares.
              num_personas_ug = sum(bin_ug, na.rm = TRUE),
              num_ocupados = sum(bin_occup * bin_ug, na.rm = TRUE),
              num_cotizantes = sum(bin_cotiza * bin_ug, na.rm = TRUE),
              bin_hogar_pensionado = max(bin_pensionado * bin_ug, na.rm = TRUE)) |>
    mutate(
      # Tasa de dependencia: dependientes (menores de 18 y mayores de 65) por
      # cada persona en edad de trabajar (18 a 65), que son quienes pueden
      # generar ingreso para sostenerlos.
      prop_dependencia = (num_minors + num_old) / na_if(num_edad_trabajar, 0),
      prop_inactividad = num_inactivos / na_if(num_edad_trabajar, 0),
      # Sin miembros de 18 a 65 años, ambas tasas no están
      # definidas y se reemplazan por 0. Estos hogares siguen siendo
      # identificables porque tienen num_edad_trabajar = 0.
      prop_dependencia = replace_na(prop_dependencia, 0),
      prop_inactividad = replace_na(prop_inactividad, 0),
      # Proporción de ocupados: personas que generan ingreso laboral por cada
      # persona que lo consume. El denominador es la unidad de gasto, el mismo
      # que usa el DANE para el ingreso per cápita con el que define la
      # pobreza. Nunca es cero: el jefe del hogar siempre pertenece a la
      # unidad de gasto.
      prop_ocupados = num_ocupados / num_personas_ug
    ) |>
    select(id, bin_head_male, num_minors, num_old, num_inactivos,
           num_edad_trabajar, prop_dependencia, prop_inactividad,
           cat_maxEduc, num_personas_ug, num_ocupados, prop_ocupados,
           num_cotizantes, bin_hogar_pensionado) |>
    ungroup()
  
  return(db_result)
}
