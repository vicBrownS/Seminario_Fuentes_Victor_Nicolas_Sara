######################################
############
########
######## Se ejecuta solo una vez
########
library(shiny)
source("Input\\Funciones.R")
datasets <- list.files("Input\\data")
claseplot <- c("Boxplot", "Histograma", "Density")
###########
#####################################
#UI------------------------------
ui <- fluidPage(
  titlePanel(h2("Visor de datos")),
  sidebarLayout(
    sidebarPanel(
      helpText(h4("El visor de datos tiene la finalidad de poder mostrar diferentes 
               gráficas en función de los parametros introducidos en los siguientes
               widgets")),
      helpText(h5("Primer dataset a representar")),
      selectInput("dataset1", "Dataset 1", 
                  choices = datasets),
      helpText(h5("Segundo dataset a representar")),
      selectInput("dataset2", "Dataset 2", 
                  choices = datasets),
      helpText(h5("Permite seleccionar el tipo de gráfico a representar")),
      selectInput("claseplot", "Tipo de gráfico",
                  choices = claseplot),
      helpText(h5("Selecciona el valor x")),
      selectInput("valuex", "Eje x", 
                  choices = "N/A"),
      helpText(h5("Selecciona el valor y")),
      selectInput("valuey", "Eje y", 
                  choices = "N/A"),
    ),
    mainPanel(

    )
  )
)
#SERVER------------------------------------
server <- function(input, output, session){
    vectores_disponibles <- reactive({
      dataset_seleccionado_1 <- read.csv(paste("Input\\data\\", input$dataset1, sep = ""), sep = ";")
      dataset_seleccionado_1 <- TDatosEjercicioFisico(dataset_seleccionado_1)
      dataset_seleccionado_2 <- read.csv(paste("Input\\data\\", input$dataset2, sep = ""), sep = ";")
      dataset_seleccionado_2 <- TDatosEjercicioFisico(dataset_seleccionado_2)
      vector <- unique(c(colnames(dataset_seleccionado_1),colnames(dataset_seleccionado_2)))
      vector
    })
    observe({
      updateSelectInput(session = session, inputId = "valuex", choices = vectores_disponibles())
      updateSelectInput(session = session, inputId = "valuey", choices = vectores_disponibles())
    })
    
}
#LLAMADA--------------------------------------
shinyApp(ui <- ui, server <- server)