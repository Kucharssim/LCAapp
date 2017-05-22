shinyServer(function(input, output) {
  data <- reactive({    
    inFile <- input$file
    
    if(is.null(inFile)){
      data <- read.csv('example.csv')
    } else {
      data <- read.csv(inFile$datapath)
    }
  })
  
  tab.d <- reactive({
    reshapeData(data())
  })

  output$data <- DT::renderDataTable(
    data(),
    selection = list(target="column")
  )
    
  output$summary <- renderUI({
    summ <- lapply(tab.d(), function(item){
      Frequency <- colSums(item)
      `Rel. Frequency` <- round(Frequency/nrow(item), 2)
      as.data.frame(rbind(Frequency, `Rel. Frequency`))
      })

    summ <- sapply(summ, function(item){
      print(item)
      knitr::kable(item, "html", caption="i1")
      # print(
      #   xtable::xtable(item), 
      #   type="html"#,
      #   #html.table.attributes='class="data table table-bordered table-condensed"'
      #   )
      }, simplify = FALSE, USE.NAMES = TRUE)
    summ <- do.call(paste, summ)
    return(div(HTML(summ),class="shiny-html-output"))
  })
  
  rv <- reactiveValues()
    
  observeEvent(input$estimate, {
    tab.d <- tab.d()
    rv$models <- as.numeric(isolate(input$classes))
    rep.n <- as.numeric(isolate(input$replications))
    funLCA <- c("emLCA", "compLik", "assignProb",
                "randomTheta", "updateTheta", "tab.d", "fitMeasures")
    clusterExport(cl=cl, varlist = funLCA, envir = environment())
  
    rv$multi.fit <- multiLCA(tab.d, rv$models, rep.n)
    rv$summary.LCA <- summary.multiLCA(rv$multi.fit)
    rv$final.fit <- fitOptimal(tab.d, rv$models,
                               rv$summary.LCA$optimal,
                               rv$multi.fit)
    rv$fit.measures <- multiFitMeasures(tab.d, data(), rv$final.fit)
  })
  
  output$diag <- renderPrint({
    list(loglik=apply(rv$summary.LCA[[1]], 2, sort, TRUE),
         replicated=colSums(rv$summary.LCA[[3]]))
  })
  output$comparison <- DT::renderDataTable({
    rv$fit.measures
    
  })
  output$parameters <- renderPrint({
    rv$multi.fit
  })
})
