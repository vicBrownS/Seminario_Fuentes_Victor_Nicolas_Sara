#Cargamos las funciones
source("Funciones.R")
library(ggplot2)
#Cargamos los csv
datos_ef_2014 <- read.csv("Input\\Ejercicio2014.csv", sep = ";")
datos_ef_2020 <- read.csv("Input\\Ejercicio2020.csv", sep = ";")
#Tratamos los datos originales sacando los subconjuntos a estudiar
mujeres_ef_2014 <- TDatosEjercicioFisico(datos_ef_2014, sexo = "Mujeres")
hombres_ef_2014 <- TDatosEjercicioFisico(datos_ef_2014, sexo = "Hombres")
mujeres_ef_2020 <- TDatosEjercicioFisico(datos_ef_2020, sexo = "Mujeres")
hombres_ef_2020 <- TDatosEjercicioFisico(datos_ef_2020, sexo = "Hombres")
diff_mujeres_ef_2014 <- DiffTotal(mujeres_ef_2020,mujeres_ef_2014)
diff_hombres_ef_2014 <- DiffTotal(hombres_ef_2020,hombres_ef_2014)
#Eliminamos los datos originales
rm(datos_ef_2014,datos_ef_2020)
#Creamos las gráficas (ggplot2)
plot_mujeres_ef_2014 <- BoxplotEfTotal(mujeres_ef_2014, "2014")
plot_hombres_ef_2014 <- BoxplotEfTotal(hombres_ef_2014, "2014")
plot_mujeres_ef_2020 <- BoxplotEfTotal(mujeres_ef_2020, "2020")
plot_hombres_ef_2020 <- BoxplotEfTotal(hombres_ef_2020, "2020")
plot_hombres_ef_2014
plot_mujeres_ef_2014
plot_hombres_ef_2020
plot_mujeres_ef_2020
