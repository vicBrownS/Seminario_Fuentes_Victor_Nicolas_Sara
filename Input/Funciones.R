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

#Trata los datos que se encuntran en las tablas, está hecho para el formato
#de datos entragado por el INE en las tablas donde las categorias son sexo y Comiunidad autónoma
TratamientoDatosGeneral <- function(data, sexo = "NULL", comunidades_autonomas = comunidades_autonomas,
                                    levels_sexos = sexos){
  #Asigna a cada columna sus factores y elimina las que no queremos
  data$Sexo <- factor(x = data$Sexo, levels = levels_sexos)
  data$Comunidades.y.Ciudades.Autónomas <- factor(x = data$Comunidades.y.Ciudades.Autónomas,
                                                  levels = comunidades_autonomas)
  data$Total.Nacional <- NULL
  #Esta linea opera en la columna 3, donde siempre estaran las categorias de la respuesta
  data[,3] <- factor(x = data[,3], levels = c(data[1,3],data[2,3],data[3,3], data[4,3],data[5,3],data[6,3]))
  #Aplica un filtro y quita las columnas que no queremos
  filtro <- data[,3] != "TOTAL" & data$Comunidades.y.Ciudades.Autónomas != ""
  data <- data[filtro,]
  #Pasa el totla de character a numeric
  data$Total = as.numeric(gsub(",",".", data$Total))
  #Filtra en funcion del sexo si así se ha elegido
  if(sexo != "NULL"){
    return(data[data$Sexo == sexo,])
  }
  return(data)
}


#Devuelve la diferencia del Total entre dos conjuntos 
DiffTotal <- function(dataset1, dataset2){
  diff <- dataset1$Total - dataset2$Total
  datadiff <- dataset1
  datadiff$Total <- diff
  return(datadiff)
}


#Crea un boxplot del dataset dado con ggplot
BoxplotEfTotal <- function(datos, año, factor = "Ejercicio.físico" ){
  library(ggplot2)
  factorx <- datos[factor]
  datos$factorx <- factorx
  #Crea el boxplot con los datos entregados en los parametros
  p <- ggplot(data = datos, 
         aes(x = factorx, y = Total)) +
    geom_jitter(aes(colour = Comunidades.y.Ciudades.Autónomas)) + 
    geom_boxplot(alpha = 0.8) +
    ggtitle(paste("Ejercicio Físico", datos$Sexo[1], año)) +
    xlab("Ejercicio Físico") +
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
