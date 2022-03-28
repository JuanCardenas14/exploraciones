library(shiny)
library(dplyr)


ui <- fluidPage(
  
  titlePanel("Calculadora PAD"),
  
  h6("Juan Camilo Cárdenas"),
  
  fluidRow(
    column (6, h4("Índice de Barthel")),
    column (6, h4("Índice de Braden"))
  ),
  
  fluidRow(
    
    column(2,
           radioButtons(inputId = "feeding", label = "Alimentación", 
                        choices = c("Independiente", "Necesita ayuda", "Dependiente"),
                        selected = "Independiente"),
           radioButtons(inputId = "bathing", label = "Baño", 
                        choices = c("Independiente", "Dependiente"), 
                        selected = "Independiente"),
           radioButtons(inputId = "grooming", label = "Arreglarse", 
                        choices = c("Independiente", "Dependiente"), 
                        selected = "Independiente"),
           radioButtons(inputId = "dressing", label = "Vestido", 
                        choices = c("Independiente", "Necesita ayuda", "Dependiente"), 
                        selected = "Independiente")
           ),
    column (2,
           radioButtons(inputId = "bowel", label = "Deposiciones", 
                        choices = c("Continente", "Incontinencia ocasional / Necesita ayuda", "Incontinencia"), 
                        selected = "Continente"),
           radioButtons(inputId = "bladder", label = "Micción", 
                        choices = c("Continente", "Incontinencia ocasional", "Incontinencia"), 
                        selected = "Continente"),
           radioButtons(inputId = "toilet", label = "Retrete", 
                        choices = c("Independiente", "Necesita ayuda", "Dependiente"), 
                        selected = "Independiente")
           ),
    column (2,
           radioButtons(inputId = "transfers", label = "Transferencias", 
                        choices = c("Independiente", "Mínima", "Gran ayuda","Dependiente"), 
                        selected = "Independiente"),
           radioButtons(inputId = "mobility", label = "Deambulación", 
                        choices = c("Independiente", "Necesita ayuda", "Independiente (silla de ruedas)",
                                    "Dependiente"), 
                        selected = "Independiente"),
           radioButtons(inputId = "stairs", label = "Escaleras", 
                        choices = c("Independiente", "Necesita ayuda","Dependiente"), 
                        selected = "Independiente")
           ),
    column (3,
            radioButtons(inputId = "sensory_perception", label = "Percepción sensorial", 
                         choices = c("Sin limitaciones", "Ligeramente limitada", " Muy limitada", "Completamente limitada"), 
                         selected = "Sin limitaciones"),
            radioButtons(inputId = "moisture", label = "Humedad", 
                         choices = c("Raramente", "Ocasionalmente", "A menudo", "Constantemente"), 
                         selected = "Raramente"),
            radioButtons(inputId = "activity", label = "Actividad", 
                         choices = c("Deambula", "Deambula ocasionalmente", "En silla", "Postrado"), 
                         selected = "Deambula")
            ),
    column(3,
           radioButtons(inputId = "walking", label = "Mobilidad", 
                        choices = c("Camina frecuentemente", "Levemente limitada", "Muy limitada", "Inmóbil"), 
                        selected = "Camina frecuentemente"),
            radioButtons(inputId = "nutrition", label = "Nutrición", 
                         choices = c("Excelente", "Adecuada", "Probablemente inadecuada", "Muy pobre"), 
                         selected = "Excelente"),
            radioButtons(inputId = "friction", label = "Fricción", 
                         choices = c("Sin problema", "Problema potencial", "Problema"), 
                         selected = "Sin problema")
           )
    ),
  fluidRow(
    column(3, h4("Functional Ambulation Category")),
    column(6, h4("Downton Fall Risk Index")), 
    column(3, h4("ECOG Performance Status Scale"))
    ),
  fluidRow(
    column(3,
           radioButtons(inputId = "fac_category", label = "Categoría FAC", 
                        choices = c("Incapacidad absoluta", "Dependiente continuo", "Dependiente intermitente", "Dependiente supervisado",
                                    "Independiente en plano", "Independiente"), 
                        selected = "Independiente")),
    column(2,
           checkboxInput(inputId = "falls", label = "Caídas previas", value = FALSE),
           radioButtons(inputId = "gait_downton", label = "Marcha", 
                        choices = c("Normal", "Insegura"), 
                        selected = "Normal")
           ),
    column(2,
           checkboxGroupInput(inputId = "meds", label = "Medicamentos", 
                        choiceNames = c("Sedantes", "Diuréticos", "Otros antihipertensivos", "Antiparkisonsianos", "Antidepresivos", "Otros"),
                        choiceValues = c(1, 1, 1, 1, 1, 1))
           ),
    column(2,
           checkboxGroupInput(inputId = "sensory_deficit", label = "Déficit sensorial", 
                        choiceNames = c("Vista", "Audición", "Extremidades"), 
                        choiceValues = c(1,1, 1)),
           radioButtons(inputId = "mental_status", label = "Estado mental", 
                        choices = c("Orientado", "Confuso"), 
                        selected = "Orientado")
           ),
    column(3,
           radioButtons(inputId = "ecog", label = "ECOG", 
                        choiceNames = c("Asintomático", "Sintomático ambulatorio", "Postrado menos 50%", 
                                    "Postrado más 50%, requiere asistencia parcial", "Postrado 100%, incapacidad total"),
                        choiceValues = c(0, 1, 2, 3, 4))
           )
  ),
  fluidRow(
    h4(textOutput(outputId = "barthel")),
    h4(textOutput(outputId = "braden_unzipped")),
    h4 (textOutput(outputId = "braden")),
    h4 (textOutput(outputId = "fac")),
    h4 (textOutput(outputId = "dfri")),
    h4 (textOutput(outputId = "ecog"))
  )
)

  

server <- function(input, output) {
  
  # Barthel
  
  alimentacion <- c("Independiente" = 10, "Necesita ayuda" = 5, "Dependiente" = 0)
  bano <- c("Independiente" = 5, "Dependiente" = 0)
  arreglarse <- c("Independiente" = 5, "Dependiente" = 0)
  vestido <- c("Independiente" = 10, "Necesita ayuda" = 5, "Dependiente" = 0)
  deposiciones <- c("Continente" = 10, "Incontinencia ocasional / Necesita ayuda" = 5, "Incontinencia" = 0)
  miccion <- c("Continente" = 10, "Incontinencia ocasional" = 5, "Incontinencia" = 0)
  retrete <- c ("Independiente" = 10, "Necesita ayuda" = 5, "Dependiente" = 0)
  transferencias <- c ("Independiente" = 15, "Mínima" = 10, "Gran ayuda" = 5,"Dependiente" = 0)
  deambulacion <- c ("Independiente" = 15, "Necesita ayuda" = 10, "Independiente (silla de ruedas)" = 5,
                     "Dependiente" = 0)
  escaleras <- c ("Independiente" = 10, "Necesita ayuda" = 5,"Dependiente" = 0)
  
  output$barthel <- renderText(paste("Barthel:", barthel_score(), "/100", barthel_classification(b = barthel_score())))

  barthel_score <- reactive(
    alimentacion[input$feeding] +
    bano[input$bathing]+
    arreglarse[input$grooming] +
    vestido[input$dressing]+
    deposiciones[input$bowel]+
    miccion[input$bladder]+
    retrete[input$toilet]+
    transferencias[input$transfers]+
    deambulacion [input$mobility]+
    escaleras[input$stairs])
  
  barthel_classification <- function (b) {
    if (b >= 80) {e <- "Independiente"}
    else if (b >= 60) {e <- "Dependencia leve"}
    else if (b >= 40) {e <- "Dependencia moderada"}
    else if (b >= 20) {e <- "Dependencia grave"}
    else if (b < 20) {e <- "Dependencia total"}
    return (e)
  }
  
  # Braden 
  
  percep_sens <- c("Sin limitaciones" = 4, "Ligeramente limitada" = 3, " Muy limitada" = 2, "Completamente limitada" = 1)
  humedad <- c("Raramente" = 4, "Ocasionalmente" = 3, "A menudo" = 2, "Constantemente" = 1)
  actividad <- c("Deambula" = 4, "Deambula ocasionalmente" = 3, "En silla" = 2, "Postrado" = 1)
  mobilidad <- c("Camina frecuentemente" = 4, "Levemente limitada" = 3, "Muy limitada" = 2, "Inmóbil" = 1)
  nutricion <- c("Excelente" = 4, "Adecuada" = 3, "Probablemente inadecuada" = 2, "Muy pobre" = 1)
  friccion <- c("Sin problema" = 3, "Problema potencial" = 2, "Problema" = 1)
  
  output$braden <- renderText(paste("Braden:", braden_score(), braden_classification(b =braden_score())))
  output$braden_unzipped <- renderText(paste("Alimentación:",  alimentacion[input$feeding], 
                                             ", Baño:", bano[input$bathing],
                                             ", Arreglarse:", arreglarse[input$grooming],
                                             ", Vestido:", vestido[input$dressing], 
                                             ", Depocisiones:", deposiciones[input$bowel],
                                             ", Micción:", miccion[input$bladder], 
                                             ", Retrete:", retrete[input$toilet],
                                             ", Transferencias:", transferencias[input$transfers],
                                             ", Deambulación:", deambulacion [input$mobility], 
                                             ", Escaleras:", escaleras[input$stairs]
                                             )
                                       )
  
  braden_score <- reactive(
    percep_sens[input$sensory_perception] +
    humedad[input$moisture]+
    actividad[input$activity]+
    mobilidad[input$walking]+
    nutricion[input$nutrition]+
    friccion[input$friction])
  
  braden_classification <- function (b) {
    if (b <= 12) {e <- "Alto riesgo"}
    else if (b <= 15) {e <- "Moderado riesgo"}
    else if (b <= 17) {e <- "Riesgo leve"}
    else if (b >= 18) {e <- "Riesgo promedio"}
    return (e)
  }
  
  # FAC scale
  
  functional_ambulation_scale <- c("Incapacidad absoluta" = 0 , "Dependiente continuo" = 1, "Dependiente intermitente" = 2,
                                   "Dependiente supervisado" = 3,  "Independiente en plano" = 4, "Independiente" = 5)
  
  FAC_escala <- reactive(functional_ambulation_scale[input$fac_category])
  
  output$fac <- renderText(paste("FAC:", FAC_escala()))
  
  
  # Downton Fall Risk Index
  
  marcha_downton <- c ("Normal" = 0, "Insegura" = 1)
  
  estado_mental_dowton <- c ("Orientado" = 0, "Confuso" = 1)
  
  falls_dfri <- reactive(as.numeric(input$falls))
  sens_def_dfri <- reactive(as.numeric(input$sensory_deficit))
  meds_dfri <- reactive(as.numeric(input$meds))
  
  
  downton_FRI <- reactive(sum(falls_dfri(), meds_dfri(), sens_def_dfri(),  
                              marcha_downton[input$gait_downton], 
                              estado_mental_dowton[input$mental_status]))
                          

  output$dfri <- renderText(paste("Downton:", downton_FRI()))
  
  # ECOG performance scale
  
  ecog_value <- reactive(as.numeric(input$ecog))

  output$ecog <- renderText(paste("ECOG:", ecog_value()))
  
  }
  

# Run the application 
shinyApp(ui = ui, server = server)

