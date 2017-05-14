emLCA <- function(d, k, start.theta=randomTheta(d, k), 
                  start.pi=gtools::rdirichlet(1, rep(1, k)),
                  tol=1e-5, output.all=TRUE){
### EM algorithm function to optimize the parameter values
###   Input: d - list of dummy variables
#            k - number of classes to estimate
#            start.theta - starting values for conditional probabilities
#            start.pi - starting values for relative sizes of classes
#            tol - tolerance level
  
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
  
  if(output.all){
    invisible(list(llik=llik.new,
                   n.iter=n.iter,
                   theta=theta,
                   pi=pi,
                   posterior=posterior,
                   starting.values=list(theta=start.theta,
                                        pi=start.pi)
                   )
              )
  } else {
    invisible(list(llik=llik.new,
                   n.iter=n.iter,
                   theta=start.theta,
                   pi=start.pi
                   )
              )
  }
}

compLik <- function(likelihoods){
### computes the sum of log-likelihoods
  #  input: matrix of unscaled likelihoods
  #  output: log-likelihood
  
  sum(log(rowSums(likelihoods)))
}

updateTheta <- function(d, posterior, theta){
### Maximisation of conditional probabilities
  # given expected class memberships and data
  #   input: d - list of dummy items
  #          posterior - prob. of class membership for each individual
  #          theta - conditional probabilities of answers
  
  # loop over items
  new.theta <- lapply(1:length(d), function(item){
    i.theta <- t(posterior) %*% d[[item]]
    return(sweep(i.theta, 1, rowSums(i.theta), "/"))
  })
  
  return(new.theta)
}


assignProb <- function(d, k, theta, pi=NA, scale=TRUE){
### Compute the individual probabilities of class membership
  # input: d - list of dummy items
  #        k - number of classes
  #        theta - list of conditional probabilities
  #        pi - proportional sizes of classes
  #        scale - return unscaled or scaled
  
  
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
### Generate random theta values
  # input: d - list of dummy items
  #        k - number of classes
  theta <- lapply(1:length(d), function(item){
    levels <- ncol(d[[item]])

    return(gtools::rdirichlet(k, rep(1, levels)))
  })
  
  return(theta)
}
