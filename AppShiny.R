######################################
############
########
######## Se ejecuta solo una vez
########
library(shiny)
library(ggplot2)
source("Input\\Funciones.R")
datasets <- list.files("Input\\data")
claseplot <- c("Boxplot", "Histograma", "Density")
###########
#####################################
#UI------------------------------
ui <- fluidPage(
  #Titulo de la pagina
  titlePanel(h2("Visor de datos")),
  #Utilizamos un sidebarLayout
  sidebarLayout(
    #El panel lateral izquierdo
    sidebarPanel(
      #Se divide el panel en dos columnas
      column(6,
        #Se escoge el primer dataset a representar
        helpText(h5("Primer dataset a representar")),
        selectInput("dataset1", "Dataset 1", 
                    choices = datasets),
        #Se escoge el valor del eje x
        helpText(h5("Selecciona el valor x")),
        selectInput("valuex", "Eje x", 
                    choices = "N/A"),
        #Se escoge el gráfico a representar
        helpText(h5("Permite seleccionar el tipo de gráfico a representar")),
        selectInput("claseplot", "Tipo de gráfico",
                    choices = claseplot)
        
      ),
      #Segunda columna
      column(6,
        #Se escoge el segundo dataset a representar
        helpText(h5("Segundo dataset a representar")),
        selectInput("dataset2", "Dataset 2", 
                    choices = datasets),
        #Se escoge el valor a representar en el eje y
        helpText(h5("Selecciona el valor y")),
        selectInput("valuey", "Eje y", 
                    choices = "N/A"),
        #Se escoge el sexo que se quiere representar en los datos
        helpText(h5("Elige el Sexo a representar")),
        selectInput("sexorepresentar", "Sexo a Representar",
                    choices = c("N/A","Hombres","Mujeres","Ambos sexos"))
      ),
      #Panel condicional que se muestra si el gráfico a representar es un boxplot
      conditionalPanel("input.claseplot == 'Boxplot'",
                      #Se escoge el elemento en el que se va a hacer el mapping
                      helpText(h5("Cambia el color de los elementos del gráfico en funcion de otra variable")),
                      radioButtons("elementomappingB", "Elemento que hace el mapping",
                                   choices = c("Jitter", "Box", "N/A"))
      ),
      #Panel condicional que se muestra si el gráfico a representar es un histograma
      conditionalPanel("input.claseplot == 'Histograma'",
                       #Slider que modifica el ancho de las bandas
                       helpText(h5("Cambia el ancho de las bandas")),
                       sliderInput("binwidth", "Ancho de banda", min = 0, max = 30, value = 5),
                       helpText(h5("Cambia el color del fill del histograma en función de una variable"))
      ),
      #Escoge la variable con la que se quiere hacer el mapping
      selectInput("variablemapping","Variable con la que se quiere hacer mapping", 
                  choices = "N/A")
      
    ),
    #Panel principal donde se inprime el gráfico
    mainPanel(
      plotOutput("plot"),
      tableOutput("tablajoin")
    )
  )
)
#SERVER------------------------------------
server <- function(input, output, session){
    #Expresion reactiva que devuelve un vector con las columnas unicas disponibles
    #que se encuentran en los datasets que se han seleccionado
    vectores_disponibles <- reactive({
      #Abre y trata los datasets seleccionados en los select de la UI
      dataset_seleccionado_1 <- read.csv(paste("Input\\data\\", input$dataset1, sep = ""), sep = ";")
      dataset_seleccionado_1 <- TratamientoDatosGeneral(dataset_seleccionado_1)
      dataset_seleccionado_2 <- read.csv(paste("Input\\data\\", input$dataset2, sep = ""), sep = ";")
      dataset_seleccionado_2 <- TratamientoDatosGeneral(dataset_seleccionado_2)
      #Crea una tabla que junta los datos de los datasets seleccionados con el objetivo de 
      #utilizarlos de manera conjunta
      if(input$dataset1 == input$dataset2){
        joindatos <- dataset_seleccionado_1
      } else{
        joindatos <- merge(x = dataset_seleccionado_1, y = dataset_seleccionado_2, by = c("Sexo","Comunidades.y.Ciudades.Autónomas"))
      }
      vector <- colnames(joindatos)
      vector
    })
    #Observer que actualiza los selects de la UI con el vector que se obtiene de la expresión 
    #reactiva anterior
    observe({
      updateSelectInput(session = session, inputId = "valuex", choices = vectores_disponibles())
      updateSelectInput(session = session, inputId = "valuey", choices = vectores_disponibles())
      updateSelectInput(session = session, inputId = "variablemapping", choices = vectores_disponibles())
      })
    #Genera el plot final
    output$plot <- renderPlot({
      #Inica todas las variables reactivas que se van a necesitar
      dataset_seleccionado_1 <- read.csv(paste("Input\\data\\", input$dataset1, sep = ""), sep = ";")
      dataset_seleccionado_1 <- TratamientoDatosGeneral(dataset_seleccionado_1)
      dataset_seleccionado_2 <- read.csv(paste("Input\\data\\", input$dataset2, sep = ""), sep = ";")
      dataset_seleccionado_2 <- TratamientoDatosGeneral(dataset_seleccionado_2)
      if(input$dataset1 == input$dataset2){
        joindatos <- dataset_seleccionado_1
      } else{
        joindatos <- merge(x = dataset_seleccionado_1, y = dataset_seleccionado_2, by = c("Sexo","Comunidades.y.Ciudades.Autónomas"))
      }
      tipografico <- input$claseplot #Tipo de gráfico a representar
      elementomappingB <- input$elementomappingB #Elemento a mapear en el boxplot
      variablemapping <- input$variablemapping #Variable con la que se hace el mapping
      binwidth <- input$binwidth #Ancho de banda del histograma
      valorx <- input$valuex #Variable del eje x
      valory <- input$valuey #Variable del eje y
      sexo_representar <- input$sexorepresentar #Sexo que se quiere representar
      #Cambia los datos en funcion del sexo que se quiere representar
      if(sexo_representar != "N/A"){
        joindatos <- joindatos[joindatos$Sexo == sexo_representar,]
      }
      #Crea el boxplot con los datos entregados en los parámetros
      p <- ggplot(data = joindatos, 
                  aes(x = .data[[valorx]] , y = .data[[valory]])) +
        xlab(valorx) + 
        ylab(valory) +
        ggtitle(paste(input$dataset1,input$dataset2, sep = "/")) +
        theme(axis.title.x = element_text(colour = "Darkblue", size = 20),
              axis.title.y = element_text(colour = "Darkblue", size = 20),
              axis.text.x = element_text(size =15, colour = "black"),
              axis.text.y = element_text(size = 15, colour = "black"),
              
              plot.title = element_text(size = 20, colour = "black", hjust = 0.5)
              )
      #Si el tipo de gráfico elgido es un boxplot
      if(tipografico == "Boxplot"){
        if(elementomappingB == "Jitter"){             #Pinta el jitter
          p + geom_boxplot(alpha = 0.8) + geom_jitter(aes(colour = .data[[variablemapping]]))
        } else if(elementomappingB == "Box"){         #Pinta las boxes
          p + geom_boxplot(aes(colour = .data[[variablemapping]]), alpha = 0.8)
        } else{
          p + geom_boxplot() + geom_jitter() #No pinta nada
        }
        #Si el tipo de gráfico es un histograma
      } else if(tipografico == "Histograma"){
        p <- ggplot(data = joindatos, 
                    aes(x = .data[[valorx]])) #Quita el valor y del gráfico para adaptarse a un histograma
        #Pinta el histograma y rellena el fill con la categoria elegida
        p + geom_histogram(binwidth = binwidth  ,color = "black", aes(fill = .data[[variablemapping]])) 
      } 
    })
  output$tablajoin <- renderTable({
    dataset_seleccionado_1 <- read.csv(paste("Input\\data\\", input$dataset1, sep = ""), sep = ";")
    dataset_seleccionado_1 <- TratamientoDatosGeneral(dataset_seleccionado_1)
    dataset_seleccionado_2 <- read.csv(paste("Input\\data\\", input$dataset2, sep = ""), sep = ";")
    dataset_seleccionado_2 <- TratamientoDatosGeneral(dataset_seleccionado_2)
    if(input$dataset1 == input$dataset2){
      joindatos <- dataset_seleccionado_1
    } else{
      joindatos <- merge(x = dataset_seleccionado_1, y = dataset_seleccionado_2, by = c("Sexo","Comunidades.y.Ciudades.Autónomas"))
    }
    if(input$sexorepresentar == "N/A"){
      joindatos
    }else {
      joindatos[joindatos$Sexo == input$sexorepresentar,]
    }
    })
    
}
#LLAMADA--------------------------------------
shinyApp(ui <- ui, server <- server)