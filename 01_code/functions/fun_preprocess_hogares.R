##########################################################
# fun_preprocess_hogares.R
#
# Función para preparar la base de hogares: selecciona las variables del
# hogar, les pone nombres descriptivos y define la variable de resultado.
# Requiere que el paquete tidyverse esté cargado.
##########################################################

fun_preprocess_hogares <- function(.db) {
  #' Preprocesar base de datos de hogares
  #'
  #' @description \code{fun_preprocess_hogares()} retorna la base de hogares
  #' con nombres descriptivos y etiquetas en sus variables categóricas. Sirve
  #' para train y para test: si la base no trae la variable \code{Pobre}
  #' (como el test), simplemente no la crea.
  #'
  #' @param .db \code{data.frame}. Base de datos de Kaggle a nivel de hogares
  #' (train_hogares.csv o test_hogares.csv).
  #'
  #' @returns \code{tibble}. Una fila por hogar con el id del hogar, \code{id};
  #' la ciudad o zona, \code{cat_ubicacion}; la tenencia de la vivienda,
  #' \code{cat_tenencia}; y, solo en train, si el hogar es pobre,
  #' \code{Pobre}.
  
  # Dominio: 23 ciudades, "RESTO URBANO" y "RURAL" (25 categorías). OJO: el
  # test no tiene hogares de BOGOTA (en train hay 10.567). Si cada base
  # creara su factor por separado, train tendría 25 niveles y test 24. Al
  # entrenar con fórmula, R corrige esto solo; pero si se arma la matriz de
  # diseño por separado (model.matrix(), necesaria para SMOTE o ROSE), las
  # columnas de train y test no coincidirían. Por eso los niveles se fijan
  # aquí, iguales para las dos bases.
  niveles_dominio <- c("ARMENIA", "BARRANQUILLA", "BOGOTA", "BUCARAMANGA",
                       "CALI", "CARTAGENA", "CUCUTA", "FLORENCIA", "IBAGUE",
                       "MANIZALES", "MEDELLIN", "MONTERIA", "NEIVA", "PASTO",
                       "PEREIRA", "POPAYAN", "QUIBDO", "RESTO URBANO",
                       "RIOHACHA", "RURAL", "SANTA MARTA", "SINCELEJO",
                       "TUNJA", "VALLEDUPAR", "VILLAVICENCIO")
  
  # Si apareciera un dominio que no está en la lista, factor() lo dejaría
  # como NA sin avisar. Mejor detenerse con un mensaje claro.
  dominios_nuevos <- setdiff(unique(.db$Dominio), niveles_dominio)
  if (length(dominios_nuevos) > 0) {
    stop("Dominios no reconocidos: ", paste(dominios_nuevos, collapse = ", "),
         call. = FALSE)
  }
  
  db_result <- .db |>
    mutate(
      cat_ubicacion = factor(Dominio, levels = niveles_dominio),
      # P5090: la vivienda ocupada por este hogar es... Se conservan las 6
      # categorías, en lugar de un indicador de "propietario" o "arrienda",
      # porque tienen niveles de pobreza muy distintos: por ejemplo, 9,8% en
      # "propia, la están pagando" frente a 48,3% en "posesión sin título".
      cat_tenencia = factor(P5090,
                            levels = 1:6,
                            labels = c("Propia pagada", "Propia pagando",
                                       "Arriendo", "Usufructo",
                                       "Posesion sin titulo", "Otra"))
    )
  
  # La variable de resultado solo existe en train.
  if ("Pobre" %in% names(.db)) {
    db_result <- db_result |>
      # 1 = pobre; 0 = no pobre. "Yes" va como primer nivel porque caret
      # trata el primer nivel del factor como la clase positiva al calcular
      # las métricas (F1, precisión, recall).
      mutate(Pobre = factor(Pobre, levels = c(1, 0), labels = c("Yes", "No"))) |>
      select(id, cat_ubicacion, cat_tenencia, Pobre)
  } else {
    db_result <- db_result |>
      select(id, cat_ubicacion, cat_tenencia)
  }
  
  return(db_result)
}
