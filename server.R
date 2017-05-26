shinyServer(function(input, output) {
  output$classes <- renderUI({
    
    max <- whichIdentified(tab.d())
    max <- ifelse(max>30, 30, max)
    selectInput("classes", "Number of classes",
                choices=as.list(1:max), multiple=TRUE)
  })
  
  output$estimate <- renderUI({
    validate(
      need(length(input$classes)>0, "Specify the models")
    )
    actionButton("estimate", "Estimate models")
  })
  
  fulldata <- reactive({    
    inFile <- input$file
    #print(inFile)
    if(is.null(inFile)){
      read.csv('example.csv')
    } else {
      #head(read.csv(inFile$datapath))
      read.csv(paste(inFile$datapath))
    }
  })
  
  data <- reactive({
    clicked <- input$datatable_columns_selected

    if(is.null(clicked)){
      fulldata()
    } else if(length(clicked)==1){
      fulldata()
    } 
    else {
      fulldata()[,clicked+1]
    }
      
  })
  
  tab.d <- reactive({
    reshapeData(data())
  })
  
  observe({
     output$datatable <- DT::renderDataTable(
      fulldata(),
      selection = list(target="column"),
      rownames = FALSE
    )
  })
  
  output$summary <- renderUI({
    validate(
      need(length(input$datatable_columns_selected) != 1,
           "Select more than one item")
    )
    
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
    tol <- input$tolerance
    rv$models <- as.numeric(isolate(input$classes))
    rep.n <- as.numeric(isolate(input$replications))
    funLCA <- c("emLCA", "compLik", "assignProb",
                "randomTheta", "updateTheta", "tab.d", "fitMeasures", "tol")
    clusterExport(cl=cl, varlist = funLCA, envir = environment())
  
    withProgress(message = "Computing the models", value=0,{
      rv$multi.fit <- multiLCA(tab.d, rv$models, rep.n, tol)
      
      incProgress(amount=1/(length(rv$models)+1), detail = "Summary statistics")
      rv$summary.LCA <- summary.multiLCA(rv$multi.fit)
      rv$final.fit <- fitOptimal(tab.d(), rv$models,
                                 rv$summary.LCA$optimal,
                                 rv$multi.fit, tol)
      rv$fit.measures <- multiFitMeasures(tab.d(), data(), rv$final.fit)
    })
    
  })
  
  output$diag <- renderPrint({
    list(loglik=apply(rv$summary.LCA[[1]], 2, sort, TRUE),
         replicated=colSums(rv$summary.LCA[[3]]))
  })
  
  output$comparison <- DT::renderDataTable(
    rv$fit.measures, rownames = FALSE, 
    selection = list(mode = "single",
                     selected = as.numeric(which.min(rv$fit.measures[,7]))
    ),
    options = list(dom = 't',
                   autoWidth=TRUE
                   )
  )
  
  selected <- reactive({
    rv$final.fit[[input$comparison_rows_selected]]
  })
  output$plotIC <- renderPlot({
    plotComparison(rv$fit.measures)
  })
  output$parameters <- renderPrint({
    selected()
  })
  
  output$plotProportions <- renderPlot({
    plotProportions(selected()$pi)
  })
  
  output$plotProbabilities <- renderPlotly({
    theta <- selected()$theta
    
    ggplotly(plotProbabilities(theta, input$WhichPlot))
  })
  
  posterior <- reactive({
    post <- selected()$posterior
    post <- data.frame(post)
    post$`Highest probability assignment` <- apply(post, 1, function(r){
      colnames(post)[which.max(r)]})
    post
  })
  
  output$class <- DT::renderDataTable(
    posterior()
  )
  
  output$Download <- downloadHandler(

    filename =  function() {
      paste0("dataWithMembership", selected()$classes, ".csv")
      },

    content = function(con) {
      d <- cbind(data(), posterior())
      write.csv(d, con)
    }
  )
  
  output$probabilities <- renderUI({
    
  })
})

