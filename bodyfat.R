# UNIVERSIDAD DE SAN CARLOS DE GUATEMALA
# FACULTAD DE AGRONOMÍA
# CENTRO DE TELEMÁTICA -CETE-
# ANÁLISIS DE REGRESIÓN LINEAL MÚLTIPLE
# P. Agr. Ludwing Isaí Marroquín Jiménez

# Base de datos: 

# Repositorio Github Analisis:
# https://github.com/Ludwing-MJ/Reg_multiple_EJ.git

# PREPARACIÓN DEL ENTORNO DE TRABAJO
# Instalación y carga de los paquetes utilizados en el análisis
if(!require("DataExplorer")) install.packages("DataExplorer")
if(!require("car")) install.packages("car")
if(!require("olsrr")) install.packages("olsrr")


# Importar base de datos
data <- read.csv("bodyfat.csv", sep = ",")


# ANÁLISIS EXPLORATORIO DE LOS DATOS 
# Revisar la estructura de la base de datos
str(data) 

# Exploración gráfica de la base de datos
plot_intro(data)
# Observación: No hay datos faltantes en la base de datos

# Elaboración de una matriz de correlaciones
plot_correlation(data)
# Observación: Se pueden identificar las variables que describen parcialmente
# la variabilidad de la variable respuesta.

# ANALISIS DE REGRESION LINEAL MULTIPLE
# Uso de Attach para no colocar el signo de dolar para llamar las variables
attach(data)
# Elaborar un modelo empleando todas las variables como predictoras
modelo_completo <- lm(BodyFat ~ Wrist + Forearm + Biceps + Ankle + Knee 
                      + Thigh + Hip + Abdomen + Chest + Neck + Height +
                        Weight + Age, data = data)
# Revisar el modelo
summary(modelo_completo)

# Aplicar stepwise (BIC como criterio)
modelo_final <- step(modelo_completo, direction = "both", k = log(nrow(data)))
summary(modelo_final)

# EVALUACIÓN DE LOS SUPUESTOS DEL MODELO FINAL

# Evaluación de linealidad
plot (modelo_final, 1) # Gráfico residuos vs predichos
# Correlaicion entre cada varaible predictora y la variable respuesta
cor.test(Abdomen, BodyFat)

# Evaluacion de normalidad
plot (modelo_final, 2) # Q-Q plot 
# Aplicar el test de normalidad de Shapiro-Wilk
shapiro.test (residuals(modelo_final))

# Evaluación de homosedasticidad
plot (modelo_final, 3) # Residuos Estudentisados vs Predichos
# Aplicar el test de homocedasticidad de Breusch-Pagan
ncvTest(modelo_final)

# Revision de ausencia de multicolinealidad
# Calcular el VIF para cada variable independiente
vif(modelo_final)

# Revision de ausencia de valores influyentes
plot (modelo_final, 4) # Cook's Distance





# Crear un nuevo dataframe solo con las variables del modelo final
subset_data <- data[, c("BodyFat", "Wrist", "Forearm", "Abdomen", "Weight")]

# Elaboración de una matriz de correlaciones
plot_correlation(subset_data)
# Crear indice entre las variables correlacionadas
data$Weight_Abdomen <- data$Weight / data$Abdomen 

# Elaborar un modelo empleando todas las variables como predictoras
modelo_completo <- lm(BodyFat ~ Wrist + Forearm + Biceps + Ankle + Knee 
                      + Hip + Thigh + Chest + Neck + Height +
                         Age + Weight_Abdomen , data = data)
# Revisar el modelo
summary(modelo_completo)

# Aplicar stepwise (BIC como criterio)
modelo_final <- step(modelo_completo, direction = "both", k = log(252))
summary(modelo_final)

# EVALUACIÓN DE LOS SUPUESTOS DEL MODELO FINAL

# Evaluación de linealidad
plot (modelo_final, 1) # Gráfico residuos vs predichos
# Correlaicion entre cada varaible predictora y la variable respuesta
cor.test(Abdomen, BodyFat)

# Evaluacion de normalidad
plot (modelo_final, 2) # Q-Q plot 
# Aplicar el test de normalidad de Shapiro-Wilk
shapiro.test (residuals(modelo_final))

# Evaluación de homosedasticidad
plot (modelo_final, 3) # Residuos Estudentisados vs Predichos
# Aplicar el test de homocedasticidad de Breusch-Pagan
ncvTest(modelo_final)

# Revision de ausencia de multicolinealidad
# Calcular el VIF para cada variable independiente
vif(modelo_final)

# Revision de ausencia de valores influyentes
plot (modelo_final, 4) # Cook's Distance



