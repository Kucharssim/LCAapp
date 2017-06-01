################################################
##### The basic functions for EM algorithm #####
################################################


emLCA <- function(d, k, start.theta=randomTheta(d, k), 
                  start.pi=gtools::rdirichlet(1, rep(1, k)),
                  tol=1e-5, output.all=TRUE){
  
  # EM algorithm function to optimize the parameter values 
  #
  # Args: 
  #   d: list of dummy variables
  #   k: number of classes to estimate
  #   start.theta: starting values for conditional probabilities
  #   start.pi: starting values for relative sizes of classes
  #   tol: tolerance level
  #   output.all: detailed output or just llik, n of iterations, and starting val
  #  
  # Returns a list:
  #   $llik: log-likelihood
  #   $n.iter: number of iterations 
  #   $classes: how many estimated classes
  #   $theta: list of conditional probabilities of responses given class
  #   $pi: a vector of class sizes
  #   $posterior: probabilities of class membership
  #   $starting.values: a list:
  #       $theta: intial values of theta
  #       $pi: initial value of pi  
  
  theta <- start.theta
  pi  <- start.pi
  
  # compute the likelihood of the responses given the initial parameters
  likelihoods <- assignProb(d, k, theta, pi, FALSE)
  
  # compute the log-likelihood of the data given the current model
  llik.old <- compLik(likelihoods)
  llik.new <- llik.old + 2*tol
  n.iter <- 0
  
  # loop until convergence of the log-likelihood 
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
  
  # Output
  if(output.all){
    names(theta) <- names(start.theta) <- names(d)
    theta <- lapply(theta, function(item){
      rownames(item) <- paste("Class", 1:k)
      item
    })
    colnames(posterior) <- paste("Class", 1:k)
    invisible(list(llik=llik.new,
                   n.iter=n.iter,
                   classes=k,
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
  # computes the sum of log-likelihoods
  #  
  # Args: 
  #   likelihoods: a matrix of unscaled likelihoods
  # Returns: log-likelihood
  
  sum(log(rowSums(likelihoods)))
}

updateTheta <- function(d, posterior, theta){
  # Maximisation of conditional probabilities
  # given expected class memberships and data
  # 
  # Args: 
  #    d: list of dummy items
  #    posterior: prob. of class membership for each individual
  #    theta: conditional probabilities of responses
  #  
  # Returns: a list of updated theta  
  
  
  # loop over items and weight the response by the probability of class
  new.theta <- lapply(1:length(d), function(item){
    i.theta <- t(posterior) %*% d[[item]]
    return(sweep(i.theta, 1, rowSums(i.theta), "/"))
  })
  
  # output
  return(new.theta)
}


assignProb <- function(d, k, theta, pi=NA, scale=TRUE){
  # Compute the individual probabilities of class membership
  #   
  # Args:
  #  d: list of dummy items
  #  k: number of classes
  #  theta: list of conditional probabilities
  #  pi: class sizes
  #  scale: return unscaled or scaled
  #  
  # Returns: a matrix with probabilities of class membership
  
  likelihood <- matrix(1, ncol=k, nrow=nrow(d[[1]]))

  # compute the likelihood for each item and integate it over items 
  for(item in 1:length(d)){
    likelihood <- likelihood * (d[[item]] %*% t(theta[[item]]))
  }
  
  # combine with baseline probabilities
  p <- sweep(likelihood, 2, pi, "*")

  # output
  if(scale){
    sweep(p, 1, rowSums(p), "/") # likelihoods sum to one
  } else{
    p
  }
}

randomTheta <- function(d, k){
  # Generate random theta values 
  #
  # Args: 
  #   d: list of dummy items
  #   k: number of classes 
  #
  # Returns a list of the random conditional probabilities  
  
  theta <- lapply(1:length(d), function(item){
    levels <- ncol(d[[item]])

    return(gtools::rdirichlet(k, rep(1, levels)))
  })
  
  return(theta)
}
