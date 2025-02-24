library(shiny)
source("check-eaf.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Schober & Clark (1989) annotation file structure test"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Annotation file ----
      fileInput("file1", "Upload your annotation file",
                accept = c("text/xml",
                           ".eaf")),
      
      # Submit button:
      actionButton("submit", "Update")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("report"),
      uiOutput("downloadErrors")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  report <- eventReactive(input$submit, {
    req(input$file1, input$file1$name)
    check.sc89.eaf(input$file1$datapath,
                     input$file1$name)
  })
  
  output$report <- renderUI({
    req(report())
    
    tagList(
      # renderText(as.character(report())),
      tags$h1("Did you pass all the checks? You should STILL double check the quality of your data"),
      tags$div("Valid filename structure:"),
      renderText(as.character(report()$`valid filename structure`)),
      tags$div("Detected student name:"),
      renderText(as.character(report()$`detected student name`)),
      # tags$div("Detected student ID number:"),
      # renderText(as.character(report()$`detected student id number`)),
      tags$div("Annotations detected on all critical tiers:"),
      renderText(as.character(report()$`annotations on all critical tiers`)),
      tags$div("Valid placement ID labels:"),
      renderText(as.character(report()$`valid placement id labels`)),
      tags$div("Valid placement ID numbers:"),
      renderText(as.character(report()$`valid placement id numbers`)),
      tags$br()
    )
  })
  

}

# Create Shiny app ----
shinyApp(ui, server)