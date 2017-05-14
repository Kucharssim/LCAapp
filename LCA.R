#library(gtools) # for rdirichlet()

emLCA <- function(d, k, start.theta=randomTheta(d, k), 
                  start.pi=gtools::rdirichlet(1, rep(1, k)), tol=1e-5){
  
  theta <- start.theta
  pi  <- start.pi
  
  likelihoods <- assignProb(d, k, theta, pi, FALSE)
  
  llik.old <- compLik(likelihoods)
  llik.new <- llik.old + 2*tol
  n.iter <- 0
  
  # loop until convergence
  while (abs(llik.new-llik.old)>tol){
    n.iter <- n.iter + 1 
    llik.old <- llik.new
    
    #expectation
    likelihoods <- assignProb(d, k, theta, pi, FALSE)
    posterior <- sweep(likelihoods, 1, rowSums(likelihoods), "/")
    
    #maximisation
    pi <- colMeans(posterior)
    theta <- updateTheta(d, posterior, theta)
    
    #log-likelihood
    llik.new <- compLik(likelihoods)
  }
  
  invisible(list(llik=llik.new,
                 n.iter=n.iter,
                 theta=theta,
                 pi=pi,
                 posterior=posterior,
                 starting.values=list(theta=start.theta,
                                      pi=start.pi)
                 )
            )
}

compLik <- function(likelihoods){
  sum(log(rowSums(likelihoods)))
}

updateTheta <- function(d, posterior, theta){
  new.theta <- lapply(1:length(d), function(item){
    #tab.d <- model.matrix(~factor(d[,item])-1)

    i.theta <- t(posterior) %*% d[[item]]
    return(sweep(i.theta, 1, rowSums(i.theta), "/"))
  })
  
  return(new.theta)
}


assignProb <- function(d, k, theta, pi=NA, scale=TRUE){

  likelihood <- matrix(1, ncol=k, nrow=nrow(d[[1]]))

  # compute the likelihood for each item and integate it over items 
  for(item in 1:length(d)){
    likelihood <- likelihood * (d[[item]] %*% t(theta[[item]]))
  }
  
  # combine with baseline probabilities
  p <- sweep(likelihood, 2, pi, "*")


  if(scale){
    sweep(p, 1, rowSums(p), "/") # likelihoods sum to one
  } else{
    p
  }
}

randomTheta <- function(d, k){
  theta <- lapply(1:length(d), function(item){
    levels <- ncol(d[[item]])

    return(gtools::rdirichlet(k, rep(1, levels)))
  })
  
  return(theta)
}


#### Run estimation in parallel ####
multiLCA <- function(d, models, rep.n, tol=1e-5){
  require(parallel)
  cores <- detectCores()
  makeCluster(cores)
  fits <- lapply(models, function(k){
    
    parLapply(cores, 1:rep.n, emLCA, d, k, tol=tol)
  })
  
}
