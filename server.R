library(shiny)
source("LCA.R")
source("DataHandling.R")

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
    
    output$diag <- renderPrint({
      
    })
  })
})
