shinyUI(navbarPage(
  title="Latent Class Analysis",
  
  ###### First Section ######
  tabPanel("Model Settings",
    sidebarLayout(
      
      # The menu
      sidebarPanel(
        fileInput("file", "Load a datafile", accept=".csv"),
        tags$br(), tags$br(),
        
        uiOutput("classes"),
        numericInput("replications", "Number of replications",
                     value = 10, min = 2),
        numericInput("tolerance", "Adjust the tolerance",
                     value = 1e-5, min = 1e-10, max = 1e-3, step = 1e-10),
        uiOutput("estimate")
        ),
      
      # Main field
      mainPanel(
        tabsetPanel(type="tabs",
          
          # Data overview
          tabPanel("Data",
                   tags$h3("Data"),
                   DT::dataTableOutput('datatable'),
                   tags$h3("Summary"),
                   uiOutput('summary')),
          
          # Diagnostisc
          tabPanel("Model diagnostics", verbatimTextOutput('diag')),
          
          # Comparison
          tabPanel("Model comparison", DT::dataTableOutput('comparison'),
                   plotOutput("plotIC")
                   ),
          
          # Details
          tabPanel("Model details", verbatimTextOutput('details'))
        )
      ) # mainPanel
    )  # sidebarLayout
  ),  # tabPanel
  
  ##### Second Section ######
  tabPanel("Output",
    tabsetPanel(type="tabs",
                
      # Plots
      tabPanel("Plots", plotOutput("plotProportions"),
               checkboxInput("WhichPlot", "Group by items"),
               plotlyOutput("plotProbabilities")),
      
      # Parameter Estimater
      tabPanel("Parameter estimates",
               downloadButton("DownloadPar",
                              "Download Parameters"),
               tags$br(), tags$br(),
               
               DT::dataTableOutput('parameters')),
      
      # Class membership
      tabPanel("Class membership",
               downloadButton("Download",
                              "Download class membership"),
               tags$br(), tags$br(),
               DT::dataTableOutput("class"))
      )
  ),  # tabPanel
  
  ###### Third Section ######
  tabPanel("About", withMathJax(), 
          fluidRow(column(2),
                   column(8,
                          includeMarkdown("documents/Intro.Rmd")
                          ),
                   column(2)
          )
  )

))
