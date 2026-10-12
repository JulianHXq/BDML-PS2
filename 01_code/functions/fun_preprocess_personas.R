##########################################################
# fun_preprocess_personas.R
#
# Función para limpiar la base de personas: renombra las variables con
# nombres descriptivos y crea indicadores a nivel persona. Adaptada de la
# Clase 7. Requiere que el paquete tidyverse esté cargado.
##########################################################

fun_preprocess_personas <- function(.db) {
  #' Preprocesar base de datos de personas
  #'
  #' @description \code{fun_preprocess_personas()} retorna una base de datos con
  #' nombres descriptivos y etiquetas en sus variables categóricas.
  #'
  #' @param .db \code{data.frame}. Base de datos de Kaggle a nivel de personas
  #' (train_personas.csv o test_personas.csv).
  #'
  #' @returns \code{tibble}. Una fila por persona con el id del hogar, \code{id},
  #' el id de la persona dentro del hogar, \code{id_persona}, y los indicadores:
  #' hombre, \code{bin_male}; jefe del hogar, \code{bin_head}; ocupado,
  #' \code{bin_occup}; inactivo, \code{bin_inactivo}; en edad de trabajar,
  #' \code{bin_edad_trabajar}; menor de edad, \code{bin_minor}; mayor de 65,
  #' \code{bin_old}; y su máximo nivel educativo, \code{cat_educ}.
  
  # Variables que usamos y sus nombres descriptivos. Todas existen tanto en
  # train como en test (verificado en los datos).
  text_vars <- c('id', 'id_persona' = 'Orden', 'num_age' = 'P6040',
                 'cat_gender' = 'P6020', 'bin_employed' = 'Oc',
                 'bin_inactivo' = 'Ina', 'cat_educ' = 'P6210',
                 'cat_relationship' = 'P6050')
  
  db_result <- .db |>
    select(all_of(text_vars)) |>
    mutate(
      # P6020: 1 = hombre, 2 = mujer.
      bin_male = ifelse(test = cat_gender == 1, yes = 1, no = 0),
      # P6050: 1 = jefe(a) del hogar.
      bin_head = ifelse(test = cat_relationship == 1, yes = 1, no = 0),
      # Oc e Ina valen 1 (sí) o NA (no), según el diccionario del DANE.
      bin_occup = ifelse(test = is.na(bin_employed), yes = 0, no = 1),
      bin_inactivo = ifelse(test = is.na(bin_inactivo), yes = 0, no = 1),
      # Grupos de edad. La edad de trabajar (18 a 65 años) es la definición
      # usada en la Clase 7.
      bin_minor = ifelse(test = num_age < 18, yes = 1, no = 0),
      bin_old = ifelse(test = num_age > 65, yes = 1, no = 0),
      bin_edad_trabajar = as.integer(between(num_age, 18, 65)),
      # P6210: 1 = ninguno, ..., 6 = superior, 9 = no sabe. Los NA son niños
      # menores de 3 años, a quienes la encuesta no pregunta.
      cat_educ = case_when(is.na(cat_educ) ~ 1, # No reportado: 'ninguno'.
                           cat_educ == 9 ~ 1,   # No sabe: 'ninguno'.
                           TRUE ~ cat_educ),    # Resto: valor original.
      # Factor ordenado: sin el orden no se puede tomar el máximo por hogar.
      cat_educ = factor(x = cat_educ,
                        levels = 1:6,
                        ordered = TRUE,
                        labels = c("Ninguno", "Preescolar", "Primaria",
                                   "Secundaria", "Media", "Universitaria"))
    ) |>
    select(id, id_persona, bin_male, bin_head, bin_occup, bin_inactivo,
           bin_edad_trabajar, cat_educ, bin_minor, bin_old)
  
  return(db_result)
}