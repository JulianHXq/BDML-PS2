##########################################################
# fun_preprocess_personas.R
#
# Función para limpiar la base de personas: renombra las variables con
# nombres descriptivos y crea indicadores a nivel persona. Requiere que el
# paquete tidyverse esté cargado.
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
  #' \code{bin_old}; su máximo nivel educativo, \code{cat_educ}; si pertenece
  #' a la unidad de gasto, \code{bin_ug}; si cotiza a pensión,
  #' \code{bin_cotiza}; y si recibió pensión el mes pasado,
  #' \code{bin_pensionado}.
  
  # Variables que usamos y sus nombres descriptivos. Todas existen tanto en
  # train como en test.
  text_vars <- c('id', 'id_persona' = 'Orden', 'num_age' = 'P6040',
                 'cat_gender' = 'P6020', 'bin_employed' = 'Oc',
                 'bin_inactivo' = 'Ina', 'cat_educ' = 'P6210',
                 'cat_relationship' = 'P6050',
                 'cat_cotiza' = 'P6920', 'cat_recibe_pension' = 'P7500s2')
  
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
      # Grupos de edad. Se considera en edad de trabajar a las personas de
      # 18 a 65 años.
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
                                   "Secundaria", "Media", "Universitaria")),
      # Unidad de gasto: personas que comparten el presupuesto del hogar. Según
      # P6050, quedan por fuera el empleado(a) del servicio (6), el
      # pensionista (7) y el trabajador (8). Así lo define el DANE: en los
      # datos, Nper - Npersug es exactamente el número de estas personas.
      bin_ug = ifelse(test = cat_relationship %in% c(6, 7, 8), yes = 0, no = 1),
      # P6920: ¿Está cotizando a un fondo de pensiones? 1 = sí, 2 = no,
      # 3 = ya es pensionado. Solo se pregunta a los ocupados: NA significa
      # que la persona no trabaja y, por lo tanto, no cotiza. Quien ya es
      # pensionado (3) tampoco cotiza; su pensión se captura con P7500s2.
      bin_cotiza = ifelse(test = cat_cotiza %in% 1, yes = 1, no = 0),
      # P7500s2: ¿El mes pasado recibió pagos por pensiones o jubilaciones?
      # 1 = sí, 2 = no, 9 = no sabe. Solo se pregunta a quien respondió "sí"
      # en P7495 (¿recibió pagos por arriendos y/o pensiones?): NA significa
      # que no recibió pensión. "No sabe" (9) se trata como no.
      bin_pensionado = ifelse(test = cat_recibe_pension %in% 1, yes = 1, no = 0)
    ) |>
    select(id, id_persona, bin_male, bin_head, bin_occup, bin_inactivo,
           bin_edad_trabajar, cat_educ, bin_minor, bin_old,
           bin_ug, bin_cotiza, bin_pensionado)
  
  return(db_result)
}
