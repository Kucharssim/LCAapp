multiLCA <- function(d, models, rep.n, tol=1e-5){
  ### Fits a n models rep.n times in parallel
  ### input: d - dummy data
  ###        models - vector of number of classes to estimate
  ###        rep.n - how many times run th replication
  ###        tol - tolerance of te emLCA algorithm
  ### output: list - replications nested within models
  ###                $llik - log.likelihood
  ###                $n.iter - number of iterations to reach the soltion
  ###                $theta, $pi - starting values
  #funLCA <- c("emLCA", "compLik", "assignProb",
  #            "randomTheta", "updateTheta", "d", "k")

  #clusterExport(cl=cl, varlist = funLCA, envir = environment())
  
  fits <- lapply(models, function(k){
    parLapply(cl, 1:rep.n, function(x){
      emLCA(d, k, tol=tol, output.all = FALSE)
    })
    
  })
  
  names(fits) <- models
  return(fits)
}

summary.multiLCA <- function(object){
  #clusterExport(cl=cl, "object")
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
  
  list(llik=llik, n.iter=n.iter, is.max=is.max, optimal=optimal)
}


fitOptimal <- function(d, models, optimal, starts){
  ### Fit optimal models
  ### input: d - dummy data
  ###        models - vector of number of classes to estimate
  ###        optimal - output $optimal of the summary.multiLCA
  ###        starts - output of multiLCA
  ### output: list of the best fit per model
  
  fits <- lapply(1:length(optimal), function(o){
    curr.optimal <- optimal[o]
    emLCA(d = d, 
          k = models[o],
          start.theta = starts[[o]][[curr.optimal]]$theta,
          start.pi = starts[[o]][[curr.optimal]]$pi)
  })
  names(fits) <- models
  return(fits)
}
