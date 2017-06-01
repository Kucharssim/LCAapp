shinyServer(function(input, output) {
  
  # Return the dropdown menu for selecting models
  output$classes <- renderUI({
    max <- whichIdentified(tab.d())
    max <- ifelse(max>30, 30, max)
    selectInput("classes", "Number of classes",
                choices=as.list(1:max), multiple=TRUE)
  })
  
  # Estimate button
  output$estimate <- renderUI({
    validate(
      need(length(input$classes)>0, "Specify the models")
    )
    actionButton("estimate", "Estimate models")
  })
  
  # Load the complete data
  fulldata <- reactive({    
    inFile <- input$file

    if(is.null(inFile)){
      read.csv('data/example.csv')
    } else {
      validate(
        need(grep("csv", inFile$name) == 1, 
             message = "Only csv files are allowed! \n
             Check documentation for details")
      )
      read.csv(paste(inFile$datapath))
    }
  })
  
  # Select the columns on which the user clicked
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
  
  # Reshape the data for the estimation function
  tab.d <- reactive({
    reshapeData(data())
  })
  
  # Return the reactive dataTable
  observe({
     output$datatable <- DT::renderDataTable(
      fulldata(),
      selection = list(target="column"),
      rownames = FALSE
    )
  })
  
  # Return the summary tables for selected variables
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
    
  # Estimate the models after clicking the estimate button
  observeEvent(input$estimate, {
    tab.d <- tab.d()
    tol <- input$tolerance
    rv$models <- as.numeric(isolate(input$classes))
    rep.n <- as.numeric(isolate(input$replications))
    funLCA <- c("emLCA", "compLik", "assignProb",
                "randomTheta", "updateTheta", "tab.d", "fitMeasures", "tol")
    clusterExport(cl=cl, varlist = funLCA, envir = environment())
  
    withProgress(message = "Computing...", value=0,{
      
      # fit multiple models at once
      rv$multi.fit <- multiLCA(tab.d, rv$models, rep.n, tol)
      
      incProgress(amount=1/(length(rv$models)+1),
                  detail = "Summary statistics")
      
      # make a summary for the model diagnostics
      rv$summary.LCA <- summary.multiLCA(rv$multi.fit)
      
      # fit the global best model
      rv$final.fit <- fitOptimal(tab.d(), rv$models,
                                 rv$summary.LCA$optimal,
                                 rv$multi.fit, tol)
      
      # compute the fit measures for all best models
      rv$fit.measures <- multiFitMeasures(tab.d(), data(), rv$final.fit)
    })
    
  })
  
  # return the model diagnostics
  output$diag <- renderPrint({
    validate(
      need(!is.null(rv$summary.LCA), "You need to estimate the models first")
    )
    
    list(replicated=colSums(rv$summary.LCA[[3]]),
         loglik=apply(rv$summary.LCA[[1]], 2, sort, TRUE)
         )
  })

  # output the reactive table for model selection
  output$comparison <- DT::renderDataTable({
    validate(
      need(!is.null(rv$fit.measures), "You need to estimate the models first")
    )
    rv$fit.measures},
    rownames = FALSE, 
    selection = list(mode = "single",
                     selected = as.numeric(which.min(rv$fit.measures[,7]))
    ),
    options = list(dom = 't',
                   autoWidth=TRUE
                   )
  )

  # store the selected model
  selected <- reactive({
    validate(
      need(!is.null(input$comparison_rows_selected),
           "You need to estimate the models first")
    )
    rv$final.fit[[input$comparison_rows_selected]]
  })
  
  # return the plot of the AIC/BIC for all models
  output$plotIC <- renderPlot({
    validate(
      need(!is.null(rv$fit.measures), "You need to estimate the models first")
    )
    plotComparison(rv$fit.measures)
  })
  
  # return the details of the selected model
  output$details <- renderPrint({
    selected()
  })
  
  # return the plot of the class proportions
  output$plotProportions <- renderPlot({
    plotProportions(selected()$pi)
  })
  
  # return the plot of the conditional probabilities of responses
  output$plotProbabilities <- renderPlotly({
    theta <- selected()$theta
    
    ggplotly(plotProbabilities(theta, input$WhichPlot), 
             tooltip = c("text", "x", "fill"))
  })
  
  # store the table of probabilities of class membership
  posterior <- reactive({
    post <- selected()$posterior
    post <- data.frame(post)
    post$`Highest probability assignment` <- apply(post, 1, function(r){
      colnames(post)[which.max(r)]})
    post
  })
  
  # store the table of model parameters
  parameters <- reactive({
    exportParameters(selected()$pi, selected()$theta)
  })
  
  # return the class membership probabilities
  output$Class <- DT::renderDataTable(
    posterior()
  )

  # download the class membership probabilties  
  output$Download <- downloadHandler(

    filename =  function() {
      paste0("dataWithMembership", selected()$classes, ".csv")
      },

    content = function(con) {
      d <- cbind(data(), posterior())
      write.csv(d, con)
    }
  )
  
  # return the model parameters
  output$parameters <- DT::renderDataTable(
    parameters(),
    options = list(dom = "ft",
                   pageLength = nrow(parameters())
    )
  )
  
  # download the model parameters
  output$DownloadPar <- downloadHandler(
    
    filename = function() {
      paste0("ParameterEstimates", selected()$classes, ".csv")
    },
    
    content = function(con) {
      d <- parameters()
      write.csv(d, con)
    }
  )
  
})

