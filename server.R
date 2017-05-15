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
    
    output$data <- DT::renderDataTable(
      data,
      selection = list(target="column")
    )
    
    output$summary <- renderTable({
      foo <- lapply(tab.d, function(item){
                abs <- colSums(item)
                rel <- round(abs/nrow(item), 2)
                rbind(abs, rel)
             })
      foo
    })
    
    
    observeEvent(input$estimate, {
      models <- as.numeric(isolate(input$classes))
      rep.n <- as.numeric(isolate(input$replications))
      multi.fit <- multiLCA(tab.d, models, rep.n)
      sum.multi.fit <- summary.multiLCA(multi.fit)
      final.fit <- fitOptimal(tab.d, models, 
                              sum.multi.fit$optimal,
                              multi.fit)
      
      output$diag <- renderPrint({
        sum.multi.fit
        })
      
      output$comparison <- DT::renderDataTable(
          multiFitMeasures(tab.d, final.fit),
          options = list(paging=FALSE,
                         searching=FALSE)
      )
      
      output$parameters <- renderPrint({
        final.fit
      })
    })
  })
})
