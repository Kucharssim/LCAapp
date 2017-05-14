library(shiny)
library(parallel)
library(gtools)
source("LCA.R")
source("DataHandling.R")
source("multiLCA.R")

funLCA <- c("emLCA", "compLik", "assignProb",
            "randomTheta", "updateTheta", "tab.d", "k")

cores <- detectCores() - 1
cl <- makeCluster(cores)

shinyServer(function(input, output) {
  observe({
    inFile <- input$file
    
    if(is.null(inFile)){
      data <- read.csv('example.csv')
    } else {
      data <- read.csv(inFile$datapath)
    }
    tab.d <- reshapeData(data)
    
    output$data <- renderDataTable({
      data
    })
    
    output$summary <- renderTable({
      foo <- lapply(tab.d, function(item){
                abs <- colSums(item)
                rel <- round(abs/nrow(item), 2)
                rbind(abs, rel)
             })
      foo
    })
    
    
    observeEvent(input$estimate, {
      output$diag <- renderPrint({
        models <- as.numeric(isolate(input$classes))
        rep.n <- as.numeric(isolate(input$replications))
        multiLCA(tab.d, models, rep.n)
        })
    })
  })
})
