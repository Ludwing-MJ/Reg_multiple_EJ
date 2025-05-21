# UNIVERSIDAD DE SAN CARLOS DE GUATEMALA
# FACULTAD DE AGRONOMÍA
# CENTRO DE TELEMÁTICA -CETE-
# ANÁLISIS DE REGRESIÓN LINEAL MÚLTIPLE
# P. Agr. Ludwing Isaí Marroquín Jiménez

# Base de datos: Trujillo Sierra, E. (2022). Modelo de Regresión 
# Lineal Múltiple - Salinidad. RStudio Pubs. Recuperado de: 
# https://rstudio-pubs-static.s3.amazonaws.com/940966_d007915418ef41c7874f7316aa972543.html

# Repositorio Github Analisis:
# https://github.com/Ludwing-MJ/Reg_multiple_EJ.git

# PREPARACIÓN DEL ENTORNO DE TRABAJO
# Instalación y carga de los paquetes utilizados en el análisis
if(!require("DataExplorer")) install.packages(DataExplorer)
if(!require("car")) install.packages(car)

# Importar base de datos
data <- read.csv("Biomasa.csv", sep = ";")

# ANÁLISIS EXPLORATORIO DE LOS DATOS 
# Revisar la estructura de la base de datos
str(data) 

# Exploración gráfica de la base de datos
plot_intro(data)
# Observación: No hay datos faltantes en la base de datos

# Elaboración de una matriz de correlaciones
plot_correlation(data)

# ANALISIS DE REGRESION LINEAL MULTIPLE
# Uso de Attach para no colocar el signo de dolar para llamar las variables
attach(data)
# Elaborar un modelo empleando todas las variables como predictoras
modelo_completo <- lm(Biomasa ~ pH + Salinidad + Zinc + potasio, data = data)
# Revisar el modelo
summary(modelo_completo)

# Aplicar stepwise (BIC como criterio)
modelo_final <- step(modelo_completo, direction = "both", k = log(nrow(data)))
summary(modelo_final)

# EVALUACIÓN DE LOS SUPUESTOS DEL MODELO FINAL

# Evaluación de linealidad
plot (modelo_final, 1) # Gráfico residuos vs predichos

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
# Calcular el VIF para cada variable independiente
plot (modelo_final, 4) # Cook's Distance



