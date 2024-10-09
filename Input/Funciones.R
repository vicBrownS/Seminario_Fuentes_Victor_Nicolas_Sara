#------------ VECTORES PARA EL TRATAMIENTO DE DATOS
comunidades_autonomas = c("Andalucía", "Aragón", "Asturias (Principado de)",
                          "Balears (Illes)", "Canarias", "Cantabria",
                          "Castilla y León", "Castilla-La Mancha", "Cataluña",
                          "Comunitat Valenciana", "Extremadura", "Galicia",
                          "Madrid (Comunidad de)", "Murcia (Región de)", "Navarra (Comunidad Foral de)",
                          "País Vasco", "Rioja (La)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)")
ejercicio_fisico = c("No aplicable", "Realizando tareas que requieren gran esfuerzo físico", 
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
  #Aplica un filtro y quita las columnas que no queremos
  filtro <- (data[,4] != "TOTAL" & data[,4] != "Total") & data$Comunidades.y.Ciudades.Autónomas != ""
  data <- data[filtro,]
  #Asigna a cada columna sus factores y elimina las que no queremos
  data$Sexo <- factor(x = data$Sexo, levels = levels_sexos)
  data$Comunidades.y.Ciudades.Autónomas <- factor(x = data$Comunidades.y.Ciudades.Autónomas,
                                                  levels = comunidades_autonomas)
  data$Total.Nacional <- NULL
  #Esta linea opera en la columna 3, donde siempre estaran las categorias de la respuesta
  #En funcion de la longitud del vector se infiere cuantas categorisa de respuesta existen
  if(length(data[,3]) == 228){
    data[,3] <- factor(x = data[,3], levels = c(data[1,3],data[2,3],data[3,3], data[4,3]))
  } else if(length(data[,3])  == 285) {
    data[,3] <- factor(x = data[,3], levels = c(data[1,3],data[2,3],data[3,3], data[4,3], data[5,3]))
  } else if(length(data[,3])  == 114) {
    data[,3] <- factor(x = data[,3], levels = c(data[1,3],data[2,3]))
  } else if(length(data[,3]) == 171) {
    data[,3] <- factor(x = data[,3], levels = c(data[1,3],data[2,3], data[3,3]))
  }
  
  
  #Pasa el totla de character a numeric
  data$Total = as.numeric(gsub(",",".", data$Total))
  return(data)
}


