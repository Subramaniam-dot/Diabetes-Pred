
# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)
library(rmarkdown)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Diabetes Predictor",
                           
  
                           tabPanel("Home",
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    sliderInput("num_preg", 
                 label = "Pregnancies: Number of times pregnant", 
                 value = 5,
				 min = 1, 
                 max = 10),
    sliderInput("glucose_conc", 
                 label = "Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test", 
                 value = 100,
				 min = 60, 
                 max = 200),
    numericInput("diastolic_bp", 
                 label = "BloodPressure: Diastolic blood pressure (mm Hg)", 
                 value = 72
				 ),
    numericInput("thickness", 
                 label = "Skin: ThicknessTriceps skin fold thickness (mm)", 
                 value = 35
				 ),
	numericInput("insulin", 
                 label = "Insulin: 2-Hour serum insulin (mu U/ml)", 
                 value = 0
				 ),
    numericInput("bmi", 
                 label = "BMI: Body mass index (weight in kg/(height in m)^2)", 
                 value = 33.6
				 ),
	numericInput("diab_pred", 
                 label = "DiabetesPedigreeFunction: Diabetes pedigree function", 
                 value = 0.627
				 ),
    sliderInput("age", 
                 label = "Age: Age (years)", 
                 value = 40,
				 min = 18, 
                 max = 64),
    				 
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata'), # Prediction results table
    HTML("<h4>This Server Predicts the probability for a person to have diabetes based on the given input parameters.</h4>")

  )
                           ), #tabPanel(), Home
  tabPanel("About", 
           titlePanel("About the Dataset:"), 
           div(includeMarkdown("about.md"), 
               align="justify")
  ) #tabPanel(), About
                ) # navbarPage()
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("num_preg",
               "glucose_conc",
               "diastolic_bp",
               "thickness",
			   "insulin",
			   "bmi",
			   "diab_pred",
			   "age"),
      Value = as.character(c(input$num_preg,
                             input$glucose_conc,
                             input$diastolic_bp,
                             input$thickness,
							 input$insulin,
							 input$bmi,
							 input$diab_pred,
							 input$age)),
      stringsAsFactors = FALSE)
    
    diabetes <- 0
    df <- rbind(df, diabetes)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
