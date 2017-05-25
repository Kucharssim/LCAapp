shinyUI(navbarPage(
  title="Latent Class Analysis",
  tabPanel("Model Settings",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Load a datafile", accept=".csv"),
        tags$br(), tags$br(),
        uiOutput("classes"),
        numericInput("replications", "Select the number of replications",
                     value = 10, min = 2),
        numericInput("tolerance", "Adjust the tolerance",
                     value = 1e-5, min = 1e-10, max = 1e-3, step = 1e-10),
        #actionButton("estimate", "Estimate models")
        uiOutput("estimate")
        ),
      mainPanel(
        tabsetPanel(type="tabs",
          tabPanel("Data", DT::dataTableOutput('datatable'), uiOutput('summary')),
          tabPanel("Model diagnostics", verbatimTextOutput('diag')),
          tabPanel("Model comparison", DT::dataTableOutput('comparison'),
                   plotOutput("plotIC")),
          tabPanel("Parameter estimates", verbatimTextOutput('parameters'))
        )
      ) # mainPanel
    )  # sidebarLayout
  ),  # tabPanel
  
  tabPanel("Output",
    #sidebarLayout(
    #  sidebarPanel(
        downloadButton("Download", "Download class membership"),
    #  ),
    #  mainPanel(
        tabsetPanel(type="tabs",
          tabPanel("Plots", plotOutput("plotProportions"),
                   checkboxInput("WhichPlot", "Group by items"),
                   plotlyOutput("plotProbabilities")),
          tabPanel("Parameter estimates"),
          tabPanel("Class membership",
                   dataTableOutput("class"))
          )
    #  )
    #)  # sidebarLayout
  ),  # tabPanel
  tabPanel("About"#, includeHTML(rmarkdown::render("Intro.Rmd"))
           #fluidRow(column(2),
          #          column(8, 
          #                 includeHTML("Intro.html")
          #                 ),
          #          column(2)
          # )
  )

))
