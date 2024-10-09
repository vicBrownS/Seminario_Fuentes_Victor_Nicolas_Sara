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
                    choices = c("N/A","Hombres","Mujeres","Ambos sexos", "Hombres/Mujeres"))
      ),
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
                       sliderInput("binwidth", "Ancho de banda", min = 1, max = 30, value = 5),
                       helpText(h5("Cambia el color del fill del histograma en función de una variable"))
      ),
      #Panel condicional que aparece si el gráfico a representar es un density chart
      conditionalPanel("input.claseplot == 'Density'",
                       helpText("Modifica la posicion de las areas de densidad respecto a las demás"),
                       selectInput("densityposition","Posicion", choices = c("stack", "fill", "dodge"))
                       ),
      #Escoge la variable con la que se quiere hacer el mapping
      selectInput("variablemapping","Variable con la que se quiere hacer mapping", 
                  choices = "N/A")
      
    ),
    #Panel principal donde se inprime el gráfico
    mainPanel(
      #Barra de navegación para acceder a los distintos elementos
      navbarPage(
        title = strong("VISTA"),
        #Panel donde se observa el plot
        tabPanel(title = "Plot",
                 plotOutput("plot"),
                 column(width = 6, selectizeInput("viewop","Opciones de Vista",
                                                 choices = list("Simplificar eje x" = "simpx",
                                                                "Simplificar eje y" = "simpy"),
                                                 multiple = T)),
                 column(width = 6, helpText("Guarda el gráfico en la carpeta de OutputPlot
                                            Para ver el display de las gráficas ir al tab 'Guardados'")
                        , actionButton("save", "Guardar Grafico",),
                        textInput("filename", "Nombre del archivo", value = "plot 0")),
        ),
        #Panel donde se ve el resumen
        tabPanel(title = "Resumen",  verbatimTextOutput("resumen")),
        #Panel donde se ven los datos
        tabPanel(title = "Data", tableOutput("tablajoin")),
        #Panel donde se ve la relacion entre los niveles y sus respectivos numeros
        # una vez hecha la simplificación de los ejes
        tabPanel(title = "Levels", h3("Leyenda de simplificacion eje x"),
                 verbatimTextOutput("summaryx"),
                 h3("Leyenda de simplificacion eje y"),
                 verbatimTextOutput("summaryy")),
        tabPanel(title = "Guardados",uiOutput("imagenes"))
      )
    )
  )
)
#SERVER------------------------------------
server <- function(input, output, session){
    #Expresion reactiva que genera el conjunto de datos que sale de hacer un join de los dos datasets
    joindatos <- reactive({
      #Abre y trata los datasets seleccionados en los select de la UI
      dataset_seleccionado_1 <- read.csv(paste("Input\\data\\", input$dataset1, sep = ""), sep = ";")
      dataset_seleccionado_1 <- TratamientoDatosGeneral(dataset_seleccionado_1)
      dataset_seleccionado_2 <- read.csv(paste("Input\\data\\", input$dataset2, sep = ""), sep = ";")
      dataset_seleccionado_2 <- TratamientoDatosGeneral(dataset_seleccionado_2)
      #Si los dos sets de datos son el mismo, solo devuelve uno de ellos
      if(input$dataset1 == input$dataset2){
        joindatos <- dataset_seleccionado_1
      } else{ #En caso contrario une los datasets utilizando cbind()
        joindatos <- cbind(x = dataset_seleccionado_1, y = dataset_seleccionado_2)
        #Elimina las columnas extras
        joindatos$y.Sexo <- NULL 
        joindatos$y.Comunidades.y.Ciudades.Autónomas <- NULL
      }
      #Cambia los datos en función del sexo que se quiere representar
      if(input$sexorepresentar == "N/A"){
        joindatos
      }else if(input$sexorepresentar == "Hombres/Mujeres"){
        joindatos[joindatos[,1] == "Hombres" | joindatos[,1] == "Mujeres",]
      }else {
        joindatos[joindatos[,1] == input$sexorepresentar,]
      }
    })
    #Expresion reactiva que devuelve un vector con las columnas unicas disponibles
    #que se encuentran en los datasets que se han seleccionado
    vectores_disponibles <- reactive({
      colnames(joindatos())
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
      joindatos <- joindatos()
      tipografico <- input$claseplot #Tipo de gráfico a representar
      elementomappingB <- input$elementomappingB #Elemento a mapear en el boxplot
      variablemapping <- input$variablemapping #Variable con la que se hace el mapping
      binwidth <- input$binwidth #Ancho de banda del histograma
      valorx <- input$valuex #Variable del eje x
      valory <- input$valuey #Variable del eje y
      sexo_representar <- input$sexorepresentar #Sexo que se quiere representar
      #Simplifica los levels del ejex o del ejey si asi se escoge
      if("simpx" %in% input$viewop){
       levels(joindatos[[valorx]]) <- seq(1,length(joindatos[[valorx]]))
      } 
      if("simpy" %in% input$viewop){
        levels(joindatos[[valory]]) <- seq(1,length(joindatos[[valory]]))
      }
      #Inicializa la gráfica con los datos elegidos e inserta los labels de los ejes
      p <- ggplot(data = joindatos, 
                  aes(x = .data[[valorx]] , y = .data[[valory]])) +
        xlab(valorx) + 
        ylab(valory) +
        #Cambia el color y tamaño de los ejes
        theme(axis.title.x = element_text(colour = "Darkblue", size = 20),
              axis.title.y = element_text(colour = "Darkblue", size = 20),
              axis.text.x = element_text(size =15, colour = "black"),
              axis.text.y = element_text(size = 15, colour = "black"),
              
              plot.title = element_text(size = 20, colour = "black", hjust = 0.5)
              )
      #Determina el título de la gráfica
      if(input$dataset1 == input$dataset2){
        p <- p + ggtitle(input$dataset1)
      } else{
        p <- p + ggtitle(paste(input$dataset1, input$dataset2, sep = "/"))
      }
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
      } else if(tipografico == "Density"){
        p <- ggplot(data = joindatos, 
                    aes(x = .data[[valorx]]))
       p + geom_density(aes(fill = .data[[variablemapping]]), position = input$densityposition)
      }
    })
    #Genera un output con los datos que se están representando en forma de tabla
   output$tablajoin <- renderTable({
    joindatos()
    })
   #Genera un output con el resumen de los datos que se representan
   output$resumen <- renderPrint({
    summary(joindatos())
    }) 
   #Genera un output que muestra la relacion entre los números 
   #de simplificación en el eje x y sus valores en el dataset
   output$summaryx <- renderPrint({
     valorx <- input$valuex
     x <- 1
     for(r in levels(joindatos()[[valorx]])){
       print(paste(x,r, sep = " ------> "))
       x <- x + 1
     }
   }
   #Genera un output que muestra la relacion entre los números 
   #de simplificación en el eje y y sus valores en el dataset
   )
   output$summaryy <- renderPrint({
     valory <- input$valuey
     x <- 1
     for(r in levels(joindatos()[[valory]])){
       print(paste(x,r, sep = " ------> "))
       x <- x + 1
     }
   })
   #Observador que guarda el último plot impreso en la carpeta designada
   # para que se active hay que presionar el botón de guardar
   observe({
     ggsave(filename = paste(input$filename,".png"), path = "Output\\OutputPlot")
     updateTextInput(session,"filename", value = paste("plot",input$save))
   }) |> bindEvent(input$save)
   
   imagename <- reactive({list.files("Output\\OutputPlot")})
   observe({
     for (name in imagename()){
       output[[name]] <- 
         renderImage({
           path <- paste("Output\\OutputPlot\\", name, sep = "")
           list(src = path, contentType = "image/png")
         }, deleteFile = F)
     } 
   })
   output$imagenes <- renderUI({
     for(name in imagename()){
       imageOutput(name)
     }
   }) |> bindEvent(input$save)
   
}
#LLAMADA--------------------------------------
shinyApp(ui <- ui, server <- server)