##########################################################
# 04_descriptive_analysis.R
#
# Objetivo: Analizar la variable de resultado: distribución, grado de desbalance
# de clases y relación con los predictores candidatos.
#
# Etapa del Problem Set: Etapa 1: Understanding the Data.
# Insumos:  03_temp/train_clean.rds.
# Salidas:  02_outputs/figures/ y 02_outputs/tables/.
##########################################################

# Código pendiente.

train <- readRDS("03_temp/train_clean.rds")
test <- readRDS("03_temp/test_clean.rds")

################## DISTRIBUCION DE LAS CLASES (POBRE Y NO POBRE DENTRO DE LA MEUSTRA)

dist_pobreza <- train %>%
  count(Pobre) %>%
  mutate(
    categoria = if_else(Pobre == 1, "Pobre", "No pobre"),
    proporcion = n / sum(n),
    etiqueta = paste0(
      percent(proporcion, accuracy = 0.1),
      " (n = ", comma(n), ")"
    )
  )


# Titularlo en la diapositiva: Classes show unbalance
ggplot(train, aes(x = factor(Pobre))) +
  geom_bar(aes(y = percent(after_stat(count / sum(count)))),
           fill = "steelblue",
           width = 0.5) +
  scale_x_discrete(labels = c("Poor", "Not Poor")) +
  labs(
    x = NULL,
    y = "Proportion of households by class"
  ) +
  theme_minimal()

########## ¿cÓMO SERÍA UN CLASIFICADOR QUE ASIGNARA TODOS A NO POBRES?

#REVISAR BIEN MAÑANA POQRQUE CREO Q LAS CLASES ESTÁN INVERTIDAS

# Clasificador ingenuo: todos son NO POBRES
real <- as.data.frame(train$Pobre)
real <- mutate(real, pred = 1, Pobre = case_when( train$Pobre == "Yes" ~ 1,
                                          train$Pobre == "No" ~ 0))

# Matriz de confusión
TP <- sum(real$Pobre == 0 & real$pred == 0, na.rm = TRUE)
TN <- sum(real$Pobre == 1 & real$pred == 1, na.rm = TRUE)
FP <- sum(real$Pobre == 1 & real$pred == 0, na.rm = TRUE)
FN <- sum(real$Pobre == 0 & real$pred== 1, na.rm = TRUE)

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
ggplot(balance, aes(x = reorder(Variable, abs(SMD)),
                    y = abs(SMD))) +
  geom_col(fill = "steelblue", width = 0.5) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Standardized mean differences (SMD)"
  ) +
  theme_minimal()

### Variación de Bogotá

