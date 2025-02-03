# 1. Recodificar GPA2 en una nueva variable GPAGRPS
# Cargar las librerías necesarias
library(haven)
install.packages("dplyr")
library(dplyr)

# Leer el archivo SPSS
stu11a <- read_sav("STU211a.SAV")

# Recodificar GPA2 en GPAGRPS
stu11a <- stu11a %>%
  mutate(GPAGRPS = case_when(
    gpa2 < 1.00 ~ 1,
    gpa2 >= 1.00 & gpa2 < 3.00 ~ 2,
    gpa2 >= 3.00 ~ 3
  ))

# Verificar los primeros registros
head(stu11a)

# Obtener las tablas de frecuencia de GPA2 y GPAGRPS

# Calcular frecuencias de GPA2
table_GPA2 <- table(stu11a$gpa2)
print(table_GPA2)

# Calcular frecuencias de GPAGRPS
table_GPAGRPS <- table(stu11a$GPAGRPS)
print(table_GPAGRPS)

# 2. Identificar el 10% de los estudiantes con más problemas personales (percentil 90)

# Calcular el percentil 90 de la variable de problemas personales (problem8)
percentil_90 <- quantile(stu11a$problem8, 0.90, na.rm = TRUE)

# Filtrar los estudiantes con problemas mayores o iguales al percentil 90
top_problems <- stu11a %>% filter(problem8 >= percentil_90)

# Mostrar los resultados
print(top_problems)

# 3. Calcular los cuartiles de la edad (AGE5) en el archivo STU211.SAV

# Calcular los cuartiles de la edad (AGE5)
quartiles_age <- quantile(stu11a$age5, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Mostrar los cuartiles
print(quartiles_age)

# 4. Obtener la distribución de frecuencia por nivel de clase (CLEVEL6)

# Calcular la distribución de frecuencia de CLEVEL6
table_CLEVEL6 <- table(stu11a$clevel6)

# Mostrar la tabla de frecuencias
print(table_CLEVEL6)

# Visualización de la distribución con un gráfico de barras
install.packages("ggplot2")
library(ggplot2)

ggplot(stu11a, aes(x = factor(clevel6))) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de Frecuencia por Nivel de Clase",
       x = "Nivel de Clase (CLEVEL6)",
       y = "Frecuencia") +
  theme_minimal()
