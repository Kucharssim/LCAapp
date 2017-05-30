shinyUI(navbarPage(
  title="Latent Class Analysis",
  tabPanel("Model Settings",
    sidebarLayout(
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
      mainPanel(
        tabsetPanel(type="tabs",
          tabPanel("Data",
                   tags$h3("Data"),
                   DT::dataTableOutput('datatable'),
                   tags$h3("Summary"),
                   uiOutput('summary')),
          tabPanel("Model diagnostics", verbatimTextOutput('diag')),
          tabPanel("Model comparison", DT::dataTableOutput('comparison'),
                   plotOutput("plotIC")
                   ),
          tabPanel("Model details", verbatimTextOutput('details'))
        )
      ) # mainPanel
    )  # sidebarLayout
  ),  # tabPanel
  
  tabPanel("Output",
    tabsetPanel(type="tabs",
      tabPanel("Plots", plotOutput("plotProportions"),
               checkboxInput("WhichPlot", "Group by items"),
               plotlyOutput("plotProbabilities")),
      tabPanel("Parameter estimates",
               downloadButton("DownloadPar",
                              "Download Parameters"),
               tags$br(), tags$br(),
               DT::dataTableOutput('parameters')),
      tabPanel("Class membership",
               downloadButton("Download",
                              "Download class membership"),
               tags$br(), tags$br(),
               dataTableOutput("class"))
      )
  ),  # tabPanel
  tabPanel("About", includeMarkdown("Intro.Rmd")
           #, includeHTML(rmarkdown::render("Intro.Rmd"))
           #fluidRow(column(2),
          #          column(8, 
          #                 includeHTML("Intro.html")
          #                 ),
          #          column(2)
          # )
  )

))
