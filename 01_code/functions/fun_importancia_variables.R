##########################################################
# fun_importancia_variables.R
#
# Objetivo:
#   Estimar importancia por permutación de los predictores
#   originales usando la caída del F1 en datos de evaluación.
#
# Aplicable a:
#   LPM, Elastic Net, Random Forest, XGBoost y otros modelos,
#   siempre que se facilite una función de predicción.
#
# Dependencias:
#   - fun_f1_score(), definida en fun_metricas.R.
#   - Ningún paquete adicional para esta función.
#
# Esta función NO entrena modelos ni escribe archivos.
##########################################################

#' Importancia de variables por permutación (F1)
#'
#' @description
#' Para cada predictor, mezcla sus valores entre observaciones,
#' conserva los demás predictores y mide cuánto disminuye el F1.
#' Repite el procedimiento para reducir la variabilidad aleatoria.
#'
#' @param .modelo Modelo ya entrenado.
#' @param .datos Data frame de evaluación con los predictores
#'   originales, una columna por variable cuya importancia se evaluará.
#'   Debe estar fuera del entrenamiento del modelo.
#' @param .real Etiquetas observadas (factor o character binario).
#' @param .predecir Función con argumentos (modelo, nuevos_datos)
#'   que devuelva puntuaciones numéricas de la clase positiva.
#' @param .umbral Umbral de clasificación entre 0 y 1.
#' @param .n_perm Número de permutaciones por variable (por defecto, 3).
#' @param .semilla Semilla para reproducibilidad.
#' @param .positive Etiqueta de la clase positiva (por defecto, "Yes").
#'
#' @return Data frame ordenado por caída media de F1, con:
#'   Variable, F1_original, F1_permutado, Importancia_F1,
#'   DE_importancia y Repeticiones.
#'
#' @details
#'   Importancia_F1 = F1_original - promedio(F1_permutado).
#'   Valores negativos pueden ocurrir debido al azar o a redundancia.
#'   Variables correlacionadas pueden compartir importancia.
#'   El resultado mide contribución predictiva, no causalidad.
#'
#' @examples
#' # source("01_code/functions/fun_metricas.R")
#' # importancia <- fun_importancia_variables(
#' #   .modelo = modelo_fold,
#' #   .datos = datos_valid,
#' #   .real = y_valid,
#' #   .predecir = predecir_modelo,
#' #   .umbral = 0.30,
#' #   .n_perm = 3
#' # )

fun_importancia_variables <- function(
    .modelo, .datos, .real, .predecir,
    .umbral = 0.5, .n_perm = 3L,
    .semilla = 2026, .positive = "Yes") {

  if (!exists("fun_f1_score", mode = "function")) {
    stop("Carga fun_metricas.R antes de usar esta función.")
  }
  if (!is.data.frame(.datos) || nrow(.datos) < 2L || ncol(.datos) == 0L) {
    stop(".datos debe ser un data frame con al menos 2 filas y 1 predictor.")
  }
  if (anyDuplicated(names(.datos))) {
    stop(".datos contiene nombres de columnas duplicados.")
  }
  if (!is.function(.predecir)) {
    stop(".predecir debe ser una función(modelo, nuevos_datos).")
  }
  if (length(.real) != nrow(.datos) || anyNA(.real)) {
    stop(".real debe contener una etiqueta no faltante por fila.")
  }
  clases <- unique(as.character(.real))
  if (length(clases) != 2L || !(.positive %in% clases)) {
    stop(".real debe contener dos clases, incluida .positive.")
  }
  negativa <- setdiff(clases, .positive)
  if (!is.numeric(.umbral) || length(.umbral) != 1L ||
      !is.finite(.umbral) || .umbral < 0 || .umbral > 1) {
    stop(".umbral debe ser un número entre 0 y 1.")
  }
  if (!is.numeric(.n_perm) || length(.n_perm) != 1L ||
      !is.finite(.n_perm) || .n_perm < 1 || .n_perm != as.integer(.n_perm)) {
    stop(".n_perm debe ser un entero positivo.")
  }

  real <- as.character(.real)
  evaluar_f1 <- function(datos) {
    scores <- .predecir(.modelo, datos)
    if (!is.numeric(scores) || length(scores) != nrow(datos) ||
        any(!is.finite(scores))) {
      stop(".predecir debe devolver una puntuación numérica finita por fila.")
    }
    pred <- ifelse(scores >= .umbral, .positive, negativa)
    f1 <- fun_f1_score(.actual = real,
                       .predicted = pred,
                       .positive = .positive)
    if (!is.finite(f1)) {
      stop("El F1 no está definido para las predicciones recibidas.")
    }
    as.numeric(f1)
  }

  f1_base <- evaluar_f1(.datos)
  set.seed(.semilla)

  resultados <- lapply(names(.datos), function(variable) {
    diferencias <- numeric(.n_perm)
    for (repeticion in seq_len(.n_perm)) {
      datos_permutados <- .datos
      datos_permutados[[variable]] <-
        .datos[[variable]][sample.int(nrow(.datos))]
      diferencias[repeticion] <- f1_base - evaluar_f1(datos_permutados)
    }
    data.frame(
      Variable = variable,
      F1_original = f1_base,
      F1_permutado = f1_base - mean(diferencias),
      Importancia_F1 = mean(diferencias),
      DE_importancia = if (.n_perm > 1) stats::sd(diferencias) else NA_real_,
      Repeticiones = as.integer(.n_perm),
      stringsAsFactors = FALSE
    )
  })

  resultado <- do.call(rbind, resultados)
  resultado <- resultado[order(-resultado$Importancia_F1), , drop = FALSE]
  rownames(resultado) <- NULL
  resultado
}
