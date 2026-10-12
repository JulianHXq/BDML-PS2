##########################################################
# 06_model_lpm.R
#
# Objetivo: Entrenar variantes de Modelo Lineal de Probabilidad (LPM) y generar sus
# predicciones para Kaggle.
#
# Etapa del Problem Set: Etapa 2: Training and Comparing Algorithms.
# Insumos:  03_temp/ (bases de modelación y particiones de validación cruzada).
# Salidas:  02_outputs/predictions/ (un .csv por variante), una fila por variante en
#           02_outputs/tables/registro_modelos.csv y el modelo en 03_temp/.
##########################################################
#
# Este script se puede correr solo (no requiere el script maestro), siempre
# que antes se hayan corrido los scripts 01 a 05. Así se puede iterar con
# Kaggle sin repetir la preparación de los datos.
#
# Estructura de cada variante (un envío a Kaggle por variante):
#   1. Entrenar con train() usando las particiones de 03_temp/cv_folds.rds.
#   2. Elegir el umbral que maximiza F1 en las predicciones fuera de muestra.
#   3. Predecir probabilidades en el test.
#   4. Guardar el envío con fun_save_submission().
##########################################################

# Preparación -------------------------------------------------------------
# Código pendiente: cargar paquetes, funciones de 01_code/functions/,
# datos de 03_temp/ y particiones de validación cruzada.

# Variante 1 --------------------------------------------------------------
# Código pendiente.
