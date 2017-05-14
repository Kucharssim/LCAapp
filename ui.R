library(shiny)

shinyUI(navbarPage(
  title="Latent Class Analysis",
  tabPanel("Model Settings",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Load a datafile", accept=".csv"),
        tags$br(), tags$br(),
        selectInput("classes", "Number of classes",
                    choices=as.list(1:10), multiple=TRUE),
        numericInput("replications", "Select the number of replications",
                     value=10, min=0),
        actionButton("estimate", "Estimate models")
        ),
      mainPanel(
        tabsetPanel(type="tabs",
          tabPanel("Data", dataTableOutput('data'), tableOutput('summary')),
          tabPanel("Model diagnostics"),
          tabPanel("Model comparison"),
          tabPanel("Parameter estimates")
        )
        # plotOutput("blabla")
      )
    )  # sidebarLayout
  ),  # tabPanel
  
  tabPanel("Output",
    sidebarLayout(
      sidebarPanel(
        selectInput("PlotModel", "Select the model", choices = as.list(1:10)),
        checkboxGroupInput("WhichPlot", "How to plot?", 
                           choices = list("Item-wise", "Class-wise")),
        downloadButton("Download", "Download class membership")
      ),
      mainPanel(
        tabsetPanel(type="tabs",
          tabPanel("Plots"),
          tabPanel("Parameter estimates"),
          tabPanel("Class membership")
          )
      )
    )  # sidebarLayout
  ),  # tabPanel
  tabPanel("About")

))
