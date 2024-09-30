#------------ VECTORES PARA EL TRATAMIENTO DE DATOS
comunidades_autonomas = c("Andalucía", "Aragón", "Asturias (Principado de)",
                          "Balears (Ille)", "Canarias", "Cantabria",
                          "Castilla y León", "Castilla-La Mancha", "Cataluña",
                          "Comunitat Valenciana", "Extremadura", "Galicia",
                          "Madrid (Comunidad de)", "Murcia (Región de)", "Navarra (Comunidad Foral de)",
                          "País Vasco", "Rioja (La)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)")
ejercicio_fisico = c("TOTAL","No aplicable", "Realizando tareas que requieren gran esfuerzo físico", 
                     "Caminando,llevando algún peso, efectuando desplazamientos frecuentes",
                     "De pie la mayor parte de la jornada sin efectuar grandes desplazamientos o esfuerzos",
                     "Sentado/a la mayor parte del día")
salud_mental = c("TOTAL","Ninguna","Moderada","Leve","Moderadamente grave", "Grave")
sexos = c("Mujeres","Hombres", "Ambos sexos")
levels_ef = c("TOTAL","NAp","1", "2", "3", "4")
#------------ FUNCIONES PARA EL TRATAMIENTO DE DATOS

#Trata las tablas de datos de Ejercicio físico sacadas del INE
TDatosEjercicioFisico <- function(data, sexo = "NULL", comunidades_autonomas = comunidades_autonomas,
                                  ef = ejercicio_fisico, levels = levels_ef, levels_sexos = sexos){
  #Añade y distribuye levels a las columnas de la tabla
  data$Sexo <- factor(x = data$Sexo, levels = levels_sexos)
  data$Comunidades.y.Ciudades.Autónomas <- factor(x = data$Comunidades.y.Ciudades.Autónomas,
                                                  levels = comunidades_autonomas)
  data$Total.Nacional <- NULL
  data$Ejercicio.físico <- factor(x = data$Ejercicio.físico, levels = ef)
  #Cambia el nombre a los levels de Ejercicio fisico
  levels(data$Ejercicio.físico) <- levels_ef
  #Filtra las filas que no son importantes
  filtro <- data$Ejercicio.físico != "TOTAL" & data$Comunidades.y.Ciudades.Autónomas != ""
  data <- data[filtro,]
  #Cambia la columna Total de character a numerico
  data$Total = as.numeric(gsub(",",".", data$Total))
  #Filtra en funcion del sexo si así se ha elegido
  if(sexo != "NULL"){
    return(data[data$Sexo == sexo,])
  } 
  return(data)
}
TDatosSaludMental <- function(data, sexo = "NULL", comunidades_autonomas = comunidades_autonomas,
                                  sm = salud_mental, levels_sexos = sexos){
  #Añade y distribuye levels a las columnas de la tabla
  data$Sexo <- factor(x = data$Sexo, levels = levels_sexos)
  data$Comunidades.y.Ciudades.Autónomas <- factor(x = data$Comunidades.y.Ciudades.Autónomas,
                                                  levels = comunidades_autonomas)
  data$Total.Nacional <- NULL
  data$Intensidad.depresión <- factor(x = data$Intensidad.depresión, levels = sm)
  #Filtra las filas que no son importantes
  filtro <- data$Intensidad.depresión != "TOTAL" & data$Comunidades.y.Ciudades.Autónomas != ""
  data <- data[filtro,]
  #Cambia la columna Total de character a numerico
  data$Total = as.numeric(gsub(",",".", data$Total))
  #Filtra en funcion del sexo si así se ha elegido
  if(sexo != "NULL"){
    return(data[data$Sexo == sexo,])
  } 
  return(data)
}

#Devuelve la diferencia del Total entre dos conjuntos de EF
DiffTotal <- function(dataset1, dataset2){
  diff <- dataset1$Total - dataset2$Total
  datadiff <- dataset1
  datadiff$Total <- diff
  return(datadiff)
}

#Crea un boxplot del dataset dado con ggplot
BoxplotEfTotal <- function(datos, año){
  library(ggplot2)
  #Crea el boxplot con los datos entregados en los parametros
  p <- ggplot(data = datos, 
         aes(x = data$Ejericio.físico, y = Total)) +
    geom_jitter(aes(colour = Comunidades.y.Ciudades.Autónomas)) + 
    geom_boxplot(alpha = 0.8) +
    ggtitle(paste("Ejercicio Físico", datos$Sexo[1], año)) +
    xlab(factor) +
    ylab("Total %") +
    theme(axis.text.x = element_text(colour = "darkblue", size = 15),
          axis.text.y = element_text(colour = "darkblue", size = 15),
          axis.title = element_text(colour = "blue", size = 18),
          
          plot.title = element_text(colour = "red", size = 20),
          
          legend.title = element_text(size = 14), 
          legend.text = element_text(size = 12)) +
    coord_cartesian(ylim = (c(0,60)))
  p$labels$colour <- "Comunidades Autónomas"
  
  return(p)
}
