shinyServer(function(input, output) {
  output$classes <- renderUI({
    max <- prod(sapply(tab.d(), ncol))
    selectInput("classes", "Number of classes",
                choices=as.list(1:max), multiple=TRUE)
  })
  
  fulldata <- reactive({    
    inFile <- input$file
    
    if(is.null(inFile)){
      read.csv('example.csv')
    } else {
      read.csv(inFile$datapath)
    }
  })
  
  data <- reactive({
    clicked <- input$datatable_columns_selected

    if(is.null(clicked)){
      fulldata()
    } else {
      fulldata()[,clicked+1]
    }
      
  })
  
  tab.d <- reactive({
    reshapeData(data())
  })

  output$datatable <- DT::renderDataTable(
    fulldata(),
    selection = list(target="column"),
    rownames = FALSE
  )
    
  output$summary <- renderUI({
    
    summ <- lapply(names(tab.d()), function(item){
      Frequency <- colSums(tab.d()[[item]])
      `Rel. Frequency` <- round(Frequency/nrow(tab.d()[[item]]), 2)
      table <- as.data.frame(rbind(Frequency, `Rel. Frequency`))
      knitr::kable(table, "html", caption=item)
      })

    summ <- do.call(paste, summ)
    return(div(HTML(summ), class="shiny-html-output"))
  })
  
  rv <- reactiveValues()
    
  observeEvent(input$estimate, {
    tab.d <- tab.d()
    rv$models <- as.numeric(isolate(input$classes))
    rep.n <- as.numeric(isolate(input$replications))
    funLCA <- c("emLCA", "compLik", "assignProb",
                "randomTheta", "updateTheta", "tab.d", "fitMeasures")
    clusterExport(cl=cl, varlist = funLCA, envir = environment())
  
    withProgress(message = "Computing the models", value=0,{
      rv$multi.fit <- multiLCA(tab.d, rv$models, rep.n)
      
      incProgress(amount=1/(length(rv$models)+1), detail = "Summary statistics")
      rv$summary.LCA <- summary.multiLCA(rv$multi.fit)
      rv$final.fit <- fitOptimal(tab.d, rv$models,
                                 rv$summary.LCA$optimal,
                                 rv$multi.fit)
      rv$fit.measures <- multiFitMeasures(tab.d, data(), rv$final.fit)
    })
    
  })
  
  output$diag <- renderPrint({
    list(loglik=apply(rv$summary.LCA[[1]], 2, sort, TRUE),
         replicated=colSums(rv$summary.LCA[[3]]))
  })
  
  output$comparison <- DT::renderDataTable(
    rv$fit.measures, rownames = FALSE, 
    selection = list(mode = "single",
                     selected = as.numeric(which.min(rv$fit.measures[,6]))
    )
  )

  output$parameters <- renderPrint({
    rv$final.fit[[input$comparison_rows_selected]]
  })
})
