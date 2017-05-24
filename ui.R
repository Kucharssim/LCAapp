shinyUI(navbarPage(
  title="Latent Class Analysis",
  tabPanel("Model Settings",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Load a datafile", accept=".csv"),
        tags$br(), tags$br(),
        uiOutput("classes"),
        # selectInput("classes", "Number of classes",
        #             choices=as.list(1:10), multiple=TRUE,
        #             selected = 1:5),
        numericInput("replications", "Select the number of replications",
                     value=10, min=0),
        actionButton("estimate", "Estimate models")
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
  tabPanel("About"#, includeHTML(rmarkdown::render("Intro.Rmd"))
           #fluidRow(column(2),
          #          column(8, 
          #                 includeHTML("Intro.html")
          #                 ),
          #          column(2)
          # )
  )

))
