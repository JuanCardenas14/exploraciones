library(shiny)
library(CVrisk)


ui <- fluidPage(
  
  titlePanel("Calculadora"),
  
  fluidRow(
    
    column(4,
           
    radioButtons(inputId = "sex", label = "Sexo", 
                choices = c("male", "female"), selected = "male"),
    
    numericInput(inputId = "age", label ="Edad", value = 30)
    
  ),
  
  
  column(4,  
    
    numericInput(inputId = "col_tot", label = "Colesterol Total",
                 value = 190),
    numericInput(inputId = "col_hdl", label = "Colesterol HDL",
                 value = 30),
    numericInput(inputId = "trigs", label = "Trigliceridos",
                 value = 150),
    textOutput(outputId = "LDL")
    
  ),
  
  column(4, 
    numericInput(inputId = "creatinine", label = "Creatinina", 
                 value = 1),
    checkboxInput("fumador", "Fumador", FALSE),
    checkboxInput("diabetico", "Diabetes", FALSE),
    numericInput(inputId = "pas", label = "Systolic blood pressure", 
                 value = 120),
    checkboxInput("bpmed", "Hypertension treatment", FALSE),
    textOutput(outputId = "framingham"), 
    textOutput(outputId = "est_gfr")
  ))
)


server <- function(input, output) {
  output$LDL <- reactive(paste( "Colesterol LDL: ",
                                (input$col_tot - input$col_hdl) - (input$trigs/5)))
  
  fumador1 <- reactive({as.numeric(input$fumador)})
  bpmed1 <- reactive ({as.numeric (input$bpmed)})
  diabetico1 <- reactive({as.numeric (input$diabetico)})
  
  
  ascvd <- reactive({ascvd_10y_frs(gender = input$sex,
                                   age = input$age,
                                   totchol = input$col_tot,
                                   hdl = input$col_hdl,
                                   sbp = input$pas,
                                   bp_med = bpmed1(),
                                   smoker = fumador1(),
                                   diabetes = diabetico1())})
  
  output$framingham <- renderText(paste("Riesgo cardiovascular (Framingham):", ascvd(), "%"))

  
  ckdepi_2021 <- function(sex, scr, age) {
    
    # define parameters depending on sex
    if (sex == "male") {k <- 0.9}
    else if (sex == "female") {k <- 0.7}
    
    if (sex == "male") {alpha <- -0.302}
    else if (sex == "female") {alpha <- -0.241}
    
    if (sex == "female") {extra <- 1.012}
    else if (sex == "male") {extra <- 1}
    
    #calcular tfg estimada
    egfr <- 142*((min(scr/k, 1))^alpha)*((max(scr/k, 1))^-1.200)*(0.9938^age)*extra
    
    return(round(egfr, 1))
  }
  
  ckdepi <- reactive(ckdepi_2021(sex = input$sex, age = input$age, scr = input$creatinine))
  
  output$est_gfr <- renderText(paste("Tasa de filtración glomerular: ",ckdepi(), "mL/min/m2"))

  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
