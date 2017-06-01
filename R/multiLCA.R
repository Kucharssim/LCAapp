multiLCA <- function(d, models, rep.n, tol=1e-5, debug=FALSE){
  # Fits a n models rep.n times in parallel
  # Args:
  #   d: dummy data
  #   models: vector of number of classes to estimate
  #   rep.n: how many times run th replication
  #   tol: tolerance of te emLCA algorithm
  #   debug: for the purpose of using the function outside shiny  
  #  
  # Returns a list: replications nested within models
  #   $llik - log.likelihood
  #   $n.iter - number of iterations to reach the soltion
  #   $theta, $pi - starting values

  # Loop over models
  fits <- lapply(models, function(k){
    if(!debug){
      # update the progress bar
      incProgress(1/(length(models)+1),
                  detail=paste(k, "classes"))
    }
    
    # Fit the replications of the model in parallel
    parLapply(cl, 1:rep.n, function(x){
      emLCA(d, k, tol=tol, output.all = FALSE)
    })
    
  })
  
  # output
  names(fits) <- models
  return(fits)
}

summary.multiLCA <- function(object){
  # Returns a summary of the multiple fit
  #
  # Args:
  #   object: a list, output from multiFit function  
  #  
  # Returns a list:
  #   $llik: log-likelihoods of the replications
  #   $n.iter: number of iterations
  #   $is.max: which replications have the highest log-likelihood   
  #   $optimal: which models are optimal 
  #             (highest log-likelihood and lowest n.iter)  
  
  llik <- sapply(object, function(o){
    sapply(o, function(i){i$llik})
  })
  
  n.iter <- sapply(object, function(o){
    sapply(o, function(i){i$n.iter})
  })
  
  is.max <- apply(round(llik, 3), 2, function(x){x==max(x)})
  
  optimal <- mapply(function(iter, max){
    which(iter %in% min(iter[max]) & max)[1]
  }, as.data.frame(n.iter), as.data.frame(is.max))
  
  # output
  list(llik=llik, n.iter=n.iter, is.max=is.max, optimal=optimal)
}


fitOptimal <- function(d, models, optimal, starts, tol){
  # Fit optimal models
  #  
  # Args: 
  #   d: dummy data
  #   models: vector of number of classes to estimate
  #   optimal: output $optimal of the summary.multiLCA
  #   starts: output of multiLCA
  #  
  # Returns a list of the best fit per model
  
  # Loop over the models and fit the best ones
  fits <- lapply(1:length(optimal), function(o){
    curr.optimal <- optimal[o]
    emLCA(d = d, 
          k = models[o],
          start.theta = starts[[o]][[curr.optimal]]$theta,
          start.pi = starts[[o]][[curr.optimal]]$pi, 
          tol = tol
          )
  })
  
  # Output
  names(fits) <- models
  return(fits)
}
