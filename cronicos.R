library(shiny)


ui <- fluidPage(
  
  titlePanel("Calculadora cronicos"),
  
  sidebarPanel(
    selectInput(inputId = "sex", label = "Sexo", 
                choices = c("Masculino", "Femenino")),
    numericInput(inputId = "age", label ="Edad", value = 0)
    
  ),
  
  
  mainPanel( 
    
    numericInput(inputId = "col_tot", label = "Colesterol Total",
                 value = 190),
    numericInput(inputId = "col_hdl", label = "Colesterol HDL",
                 value = 30),
    numericInput(inputId = "trigs", label = "Trigliceridos",
                 value = 150),
    textOutput(outputId = "LDL")
    
  ),
  
  sidebarPanel(
    numericInput(inputId = "creatinine", label = "Creatinina", 
                 value = 1),
    checkboxInput("smoker", "Fumador", FALSE)
  )
)


server <- function(input, output) {
  output$LDL <- reactive(paste( "Colesterol LDL: ",
                                (input$col_tot - input$col_hdl) - (input$trigs/5)))
}

# Run the application 
shinyApp(ui = ui, server = server)
