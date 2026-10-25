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
  #' \code{prop_ocupados}; si alguien recibe pensión,
  #' \code{bin_hogar_pensionado}; las horas trabajadas a la semana,
  #' \code{num_horas_ug}; el número de asalariados, \code{num_asalariados},
  #' y de trabajadores por cuenta propia, \code{num_cuentapropia}; la
  #' posición ocupacional del jefe, \code{cat_pos_jefe}; y el nivel educativo
  #' promedio del jefe y su cónyuge, \code{num_educ_jefe_conyuge}.
  
  db_result <- .db |>
    group_by(id) |>
    summarize(bin_head_male = max(bin_head * bin_male, na.rm = TRUE),
              num_minors = sum(bin_minor, na.rm = TRUE),
              num_old = sum(bin_old, na.rm = TRUE),
              num_inactivos = sum(bin_inactivo * bin_edad_trabajar, na.rm = TRUE),
              num_edad_trabajar = sum(bin_edad_trabajar, na.rm = TRUE),
              cat_maxEduc = max(cat_educ, na.rm = TRUE),
              # Variables de la unidad de gasto: quienes comparten el presupuesto del hogar
              # (excluye servicio doméstico, pensionistas y trabajadores).
              num_personas_ug = sum(bin_ug, na.rm = TRUE),
              num_ocupados = sum(bin_occup * bin_ug, na.rm = TRUE),
              num_cotizantes = sum(bin_cotiza * bin_ug, na.rm = TRUE),
              bin_hogar_pensionado = max(bin_pensionado * bin_ug, na.rm = TRUE),
              # Calidad del trabajo: cuánto se trabaja y en qué posición. Se
              # suman solo los miembros de la unidad de gasto, como en las
              # demás variables laborales.
              num_horas_ug = sum(num_horas * bin_ug, na.rm = TRUE),
              num_asalariados = sum(bin_asalariado * bin_ug, na.rm = TRUE),
              num_cuentapropia = sum(bin_cuentapropia * bin_ug, na.rm = TRUE),
              # Posición ocupacional del jefe. Cada hogar tiene exactamente un
              # jefe (verificado en train y en test), así que siempre hay un
              # único valor.
              cat_pos_jefe = cat_posicion[bin_head == 1],
              # Nivel educativo promedio del jefe y su cónyuge, si lo hay, con
              # el código de 1 (ninguno) a 6 (universitaria). Son los
              # principales generadores de ingreso del hogar. Como todo hogar
              # tiene jefe, la misma regla sirve para todos y nunca queda
              # vacía.
              num_educ_jefe_conyuge = mean(as.integer(
                cat_educ[bin_head == 1 | bin_conyuge == 1])),
              # PREDICTORES NUEVOS: demografía de la unidad de gasto y jefe.
              num_edad_jefe = num_age[bin_head == 1],
              num_edad_promedio_ug = mean(num_age[bin_ug == 1]),
              prop_menores_ug = sum(bin_minor * bin_ug, na.rm = TRUE) /
                num_personas_ug,
              prop_mujeres_ug = sum(bin_female * bin_ug, na.rm = TRUE) /
                num_personas_ug,
              num_menores_6_ug = sum(bin_menor_6 * bin_ug, na.rm = TRUE),
              # Educación del jefe y adultos universitarios en la UG.
              cat_educ_jefe = cat_educ[bin_head == 1],
              num_adultos_universitarios_ug = sum(
                bin_adulto_universitario * bin_ug, na.rm = TRUE),
              # Empleo, desocupación y estudiantes en la UG.
              bin_jefe_ocupado = bin_occup[bin_head == 1],
              num_desocupados_ug = sum(bin_desocupado * bin_ug, na.rm = TRUE),
              num_estudiantes_ug = sum(bin_estudiante * bin_ug, na.rm = TRUE),
              # Salud: personas que declaran no estar afiliadas. El régimen
              # (subsidiado o contributivo, P6100) no se usa: la afiliación al
              # régimen subsidiado depende en parte del SISBÉN, otro
              # instrumento de focalización.
              num_no_afiliados_salud_ug = sum(
                bin_no_afiliado_salud * bin_ug, na.rm = TRUE),
              # Calidad del empleo: subempleo, formalidad y tamaño de la
              # empresa del jefe; y transferencias de otros hogares.
              num_subempleo_ug = sum(bin_subempleo * bin_ug, na.rm = TRUE),
              bin_prima_servicios_ug = max(bin_prima_servicios * bin_ug,
                                           na.rm = TRUE),
              bin_dinero_hogares_ug = max(bin_dinero_hogares * bin_ug,
                                          na.rm = TRUE),
              cat_tam_empresa_jefe = cat_tam_empresa[bin_head == 1]) |>
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
      prop_ocupados = num_ocupados / num_personas_ug,
      # La educación se trabajó como factor ordenado solo para poder tomar el
      # máximo del hogar. Para los modelos se convierte en factor sin orden:
      # así cada nivel entra como su propia variable dicotómica. Los niveles y su orden de
      # presentación se conservan ("Ninguno" sigue siendo la categoría base).
      cat_maxEduc = factor(cat_maxEduc, ordered = FALSE),
      # PREDICTORES NUEVOS: denominadores acordes con el fenómeno medido.
      cat_educ_jefe = factor(cat_educ_jefe, ordered = FALSE),
      tasa_desempleo_ug = num_desocupados_ug /
        na_if(num_ocupados + num_desocupados_ug, 0),
      bin_sin_activos_ug = as.integer(
        num_ocupados + num_desocupados_ug == 0),
      tasa_desempleo_ug = replace_na(tasa_desempleo_ug, 0),
      prop_cotizantes_ocupados_ug = num_cotizantes / na_if(num_ocupados, 0),
      prop_cotizantes_ocupados_ug = replace_na(
        prop_cotizantes_ocupados_ug, 0)
    ) |>
    select(id, bin_head_male, num_minors, num_old, num_inactivos,
           num_edad_trabajar, prop_dependencia, prop_inactividad,
           cat_maxEduc, num_personas_ug, num_ocupados, prop_ocupados,
           num_cotizantes, bin_hogar_pensionado, num_horas_ug,
           num_asalariados, num_cuentapropia, cat_pos_jefe,
           num_educ_jefe_conyuge,
           # Predictores nuevos: se agregan sin retirar los existentes.
           num_edad_jefe, num_edad_promedio_ug, prop_menores_ug,
           prop_mujeres_ug, num_menores_6_ug,
           cat_educ_jefe, num_adultos_universitarios_ug,
           bin_jefe_ocupado, num_desocupados_ug, tasa_desempleo_ug,
           bin_sin_activos_ug, num_estudiantes_ug,
           prop_cotizantes_ocupados_ug,
           num_no_afiliados_salud_ug, num_subempleo_ug,
           bin_prima_servicios_ug, bin_dinero_hogares_ug,
           cat_tam_empresa_jefe) |>
    ungroup()
  
  return(db_result)
}


##########################################################
# AMPLIACION LIGHTGBM - SOLO CODIGO ANADIDO
# Se conserva integro todo el codigo anterior. Se amplia la MISMA funcion.
# .ampliado=FALSE mantiene exactamente el comportamiento previo.
# .ampliado=TRUE se usa exclusivamente desde 11_model_lightgbm.R.
# Los auxiliares son locales; no se crean otras funciones publicas.
##########################################################
fun_colapse_personas <- local({
  .original <- fun_colapse_personas
  function(.db, .hogares = NULL, .ampliado = FALSE) {
    if (!isTRUE(.ampliado)) return(.original(.db))
    if (is.null(.hogares) || is.null(attr(.hogares, "lgbm_household_features")) ||
        is.null(attr(.db, "lgbm_flags"))) {
      stop("Prepare hogares y personas con .ampliado=TRUE antes del colapso ampliado.")
    }
    h <- .hogares
    p <- data.table::copy(data.table::as.data.table(.db))
    ids <- h$id
    nh <- nrow(h)
    p[, hh_idx := match(id, ids)]
    stopifnot(!anyNA(p$hh_idx), all(tabulate(p$hh_idx, nbins = nh) == h$Nper))
    features <- attr(h, "lgbm_household_features")
    flags <- attr(.db, "lgbm_flags")
    grupos <- attr(.db, "lgbm_groups")
    agregar <- function(nombre_col, valor) {
      stopifnot(length(valor) == nh)
      features[[nombre_col]] <<- valor
    }
    f32 <- function(x) {
      x <- as.numeric(x)
      x[!is.finite(x)] <- NA_real_
      readBin(writeBin(x, raw(), size = 4L, endian = "little"),
              what = "numeric", n = length(x), size = 4L, endian = "little")
    }
    dividir <- function(a, b) { b[b == 0 & !is.na(b)] <- NA_real_; a / b }
    eq <- function(x, valor) { z <- x == valor; z[is.na(z)] <- FALSE; z }
    flag_names <- names(flags)
    flag_dt <- as.data.table(lapply(flags, as.numeric))
    flag_dt[, hh_idx := p$hh_idx]
    rm(flags)
    for (grupo in names(grupos)) {
      filas <- which(grupos[[grupo]])
      tamanio <- tabulate(p$hh_idx[filas], nbins = nh)
      agregar(paste0(grupo, "_n"), tamanio)
      sumas <- flag_dt[filas, lapply(.SD, sum), by = hh_idx, .SDcols = flag_names]
      pos <- match(seq_len(nh), sumas$hh_idx)
      for (c in flag_names) {
        conteo <- sumas[[c]][pos]
        conteo[is.na(conteo)] <- 0
        agregar(paste0(grupo, "_count_", c), conteo)
        agregar(paste0(grupo, "_share_", c), dividir(conteo, tamanio))
      }
      numericas <- c("P6040", "education_years", "education_level", "P6210s1")
      if (grupo %in% c("all", "ug", "work")) numericas <- c(numericas, "P6426", "P6800", "P7045")
      datos_grupo <- p[filas, c("hh_idx", numericas), with = FALSE]
      for (c in numericas) {
        expresion <- substitute(list(mean = mean(V, na.rm = TRUE), max = max(V, na.rm = TRUE),
          min = min(V, na.rm = TRUE), std = sd(V, na.rm = TRUE), sum = sum(V, na.rm = TRUE)),
          list(V = as.name(c)))
        estadisticas <- suppressWarnings(datos_grupo[, eval(expresion), by = hh_idx])
        posicion <- match(seq_len(nh), estadisticas$hh_idx)
        for (est in c("mean", "max", "min", "std", "sum")) {
          agregar(paste0(grupo, "_", c, "_", est), estadisticas[[est]][posicion])
        }
      }
      message("  Agregados ", grupo, " completados")
    }
    rm(flag_dt, datos_grupo, estadisticas, sumas, grupos)
    conteos <- fun_conteos_personas(p, .hogares = h, .ampliado = TRUE)
    stopifnot(identical(conteos$id, ids))
    for (c in setdiff(names(conteos), "id")) agregar(c, conteos[[c]])
    rm(conteos)
    persona_cols <- attr(.db, "lgbm_person_columns")
    for (rol in c("head", "partner")) {
      codigo <- if (rol == "head") 1 else 2
      filas <- which(eq(p$P6050, codigo))
      filas <- filas[!duplicated(p$id[filas])]
      pos <- match(ids, p$id[filas])
      for (c in persona_cols) agregar(paste0(rol, "_", c), p[[c]][filas][pos])
    }
    orden <- order(p$id, -p$P6040, p$P6050, na.last = TRUE, method = "radix")
    rango <- sequence(rle(p$id[orden])$lengths)
    miembro_cols <- c("P6040", "P6020", "P6050", "P6210", "P6210s1", "P6100", "P6240", "P6430", "P6800", "P6920", "Oficio", "P6870")
    for (k in 1:3) {
      filas <- orden[rango == k]
      pos <- match(ids, p$id[filas])
      for (c in miembro_cols) agregar(paste0("member_", k, "_", c), p[[c]][filas][pos])
    }
    renta <- features$hh_P5140
    faltante <- is.na(renta)
    renta[faltante] <- features$hh_P5130[faltante]
    agregar("hh_rent_combined", renta)
    for (tipo in c("rent", "mortgage")) {
      s <- if (tipo == "rent") renta else features$hh_P5100
      for (den in c("Nper", "Npersug", "P5000", "P5010", "Lp")) {
        agregar(paste0("hh_", tipo, "_per_", den), dividir(s, h[[den]]))
      }
      agregar(paste0("hh_log_", tipo), log1p(pmax(s, 0)))
      agregar(paste0("hh_", tipo, "_missing"), as.integer(is.na(s)))
    }
    agregar("hh_crowding_bedroom", dividir(h$Nper, h$P5010))
    agregar("hh_crowding_room", dividir(h$Nper, h$P5000))
    agregar("hh_nper_minus_ug", h$Nper - h$Npersug)
    agregar("hh_rent_ug_povertyline", dividir(renta, h$Npersug * h$Lp))
    for (grupo in c("all", "ug")) {
      for (tipo in c("employed", "formal_proxy", "pension_income", "health_contributive")) {
        agregar(paste0("demo_", grupo, "_persons_per_", tipo),
                dividir(h$Npersug, features[[paste0(grupo, "_count_", tipo)]]))
      }
      agregar(paste0("demo_", grupo, "_dependent_per_worker"),
        dividir(features[[paste0(grupo, "_n")]] - features[[paste0(grupo, "_count_employed")]],
                features[[paste0(grupo, "_count_employed")]]))
      agregar(paste0("demo_", grupo, "_dependent_age_ratio"),
        dividir(features[[paste0(grupo, "_count_child")]] + features[[paste0(grupo, "_count_age_65_plus")]],
                features[[paste0(grupo, "_count_working_age")]]))
    }
    agregar("demo_no_partner", as.integer(is.na(features$partner_P6040)))
    agregar("demo_head_partner_age_gap", features$head_P6040 - features$partner_P6040)
    agregar("demo_head_partner_education_gap", features$head_education_years - features$partner_education_years)
    cat_hh <- c("hh_Dominio", "hh_Depto", "hh_Clase")
    for (c in setdiff(names(features), cat_hh)) features[[c]] <- f32(features[[c]])
    stopifnot(all(features$all_n == h$Nper), all(features$ug_n == h$Npersug),
              all(features$head_P6050 == 1))
    resultado <- data.table::as.data.table(c(list(id = ids), features))
    if ("Pobre" %in% names(h)) resultado[, Pobre := as.integer(h$Pobre)]
    data.table::setattr(resultado, "lgbm_feature_columns", names(features))
    resultado
  }
})
