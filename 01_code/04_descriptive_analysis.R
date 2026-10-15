##########################################################
# 04_descriptive_analysis.R
#
# Objetivo: Analizar la variable de resultado: distribución, grado de desbalance
# de clases y relación con los predictores candidatos.
#
# Etapa del Problem Set: Etapa 1: Understanding the Data.
# Insumos:  03_temp/train_clean.rds y 03_temp/test_clean.rds.
# Salidas:  02_outputs/figures/ y 02_outputs/tables/.
##########################################################

# Paquetes ------------------------------------------------------------------
# Este script carga sus propios paquetes para poder correrse solo, una vez
# corrido 02_clean_merge_data.R.
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, scales)

train <- readRDS("03_temp/train_clean.rds")
test <- readRDS("03_temp/test_clean.rds")

################## DISTRIBUCION DE LAS CLASES (POBRE Y NO POBRE DENTRO DE LA MUESTRA)

# Pobre es un factor con niveles "Yes" (pobre) y "No" (no pobre).
dist_pobreza <- train %>%
  count(Pobre) %>%
  mutate(
    categoria = factor(if_else(Pobre == "Yes", "Poor", "Not Poor"),
                       levels = c("Poor", "Not Poor")),
    proporcion = n / sum(n),
    etiqueta = paste0(
      percent(proporcion, accuracy = 0.1),
      " (n = ", comma(n), ")"
    )
  )

# Titularlo en la diapositiva: Classes show unbalance
# El eje y debe ser numérico: el formato de porcentaje se aplica solo a las
# etiquetas del eje, para que la altura de las barras sea proporcional.
distribucion_clase <- ggplot(dist_pobreza, aes(x = categoria, y = proporcion)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent, limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL,
    y = "Proportion of households by class"
  ) +
  theme_minimal()

ggsave(
  "02_outputs/figures/distribucion_clase.pdf",
  plot =  distribucion_clase,
  width = 7,
  height = 4
)

########## ¿CÓMO SERÍA UN CLASIFICADOR QUE ASIGNARA TODOS A NO POBRES?

# Clasificador ingenuo: todos son NO POBRES
real <- data.frame(observed = train$Pobre, pred = "No")

# Matriz de confusión (la clase positiva es "Yes", pobre)
TP <- sum(real$observed == "Yes" & real$pred == "Yes")
TN <- sum(real$observed == "No" & real$pred == "No")
FP <- sum(real$observed == "No" & real$pred == "Yes")
FN <- sum(real$observed == "Yes" & real$pred == "No")

# Métricas
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
sensibilidad <- ifelse(TP + FN == 0, NA_real_, TP / (TP + FN))
especificidad <- ifelse(TN + FP == 0, NA_real_, TN / (TN + FP))

F1 <- ifelse(
  precision + sensibilidad == 0,
  0,
  2 * precision * sensibilidad / (precision + sensibilidad)
)

metricas <- data.frame(
  Metrica = c("Accuracy", "F1-score", "Sensibilidad", "Especificidad"),
  Valor = c(accuracy, F1, sensibilidad, especificidad)
)

metricas

### Volvemos estas métricas exportables a latex
valor <- function(m) {
  sprintf("%.2f", 100 * metricas$Valor[metricas$Metrica == m])
}

lineas <- c(
  paste0("\\def\\accBase{", valor("Accuracy"), "\\%}"),
  paste0("\\def\\f1Base{", valor("F1-score"), "\\%}"),
  paste0("\\def\\sensBase{", valor("Sensibilidad"), "\\%}"),
  paste0("\\def\\espBase{", valor("Especificidad"), "\\%}")
)
writeLines(lineas, "02_outputs/tables/metricas_baseline.tex")

###### DIFERENCIA DE MEDIAS ESTANDARIZADAS


# Gráfico
vars <- intersect(names(train), names(test))
vars <- vars[sapply(train[vars], is.numeric) &
               sapply(test[vars], is.numeric)]

# Calcular SMD
balance <- data.frame(
  Variable = vars,
  SMD = sapply(vars, function(v) {
    x <- train[[v]]
    y <- test[[v]]
    
    (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) /
      sqrt((var(x, na.rm = TRUE) + var(y, na.rm = TRUE)) / 2)
  })
)

# Gráfico (DIFERENCIAS MAYORES A 0.1 SON CONSIDERABLES (NO HAY EN ESTAS Xs))
SMD <- ggplot(balance, aes(x = reorder(Variable, abs(SMD)),
                           y = abs(SMD))) +
  geom_col(fill = "steelblue", width = 0.5) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Standardized mean differences (SMD)"
  ) +
  theme_minimal()

ggsave(
  "02_outputs/figures/SMD.pdf",
  plot =  SMD,
  width = 7,
  height = 4
)


########## Variación de Bogotá: Cómo cambia la prueba de balance?
train_sin <- filter(train, cat_ubicacion != "BOGOTA")

# Calcular SMD
smd <- function(x, y) {
  abs(mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) /
    sqrt((var(x, na.rm = TRUE) + var(y, na.rm = TRUE)) / 2)
}

# Comparación con y sin Bogotá
balance <- data.frame(
  Variable = vars,
  Con_Bogota = sapply(vars, function(v)
    smd(train[[v]], test[[v]])),
  Sin_Bogota = sapply(vars, function(v)
    smd(train_sin[[v]], test[[v]]))
)

# Pasar los datos a formato largo
balance_graf <- balance %>%
  pivot_longer(
    cols = c(Con_Bogota, Sin_Bogota),
    names_to = "Grupo",
    values_to = "SMD"
  )

# Gráfico de barras
comparacion_bogota <- ggplot(balance_graf, aes(
  x = reorder(Variable, SMD),
  y = SMD,
  fill = Grupo
)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0.1, linetype = "dashed",  color = "red") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Con_Bogota" = "#D97736",
      "Sin_Bogota" = "#3285BA"
    ),
    labels = c("Con Bogotá", "Sin Bogotá")
  ) +
  labs(
    subtitle = "Comparación con y sin Bogotá",
    x = NULL,
    y = "Diferencia estandarizada absoluta (SMD)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  "02_outputs/figures/comparacion_Bogota.pdf",
  plot = comparacion_bogota,
  width = 7,
  height = 4
)

# Se liberan los objetos de este script (se conservan las funciones).
rm(list = ls()[!sapply(mget(ls()), is.function)])
rm(smd, valor)
