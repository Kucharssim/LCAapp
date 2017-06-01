nParamsDf <- function(k, tab.d){
  # compute the number of estimated parameters
  # and degrees of freedom
  #  
  # Args:
  #   k: number of classes in the model
  #   tab.d: the dummy data
  #
  # Returns a vector of df and n of parameters
  
  lev <- Levels(tab.d) # vector of levels 
  
  n.params <- (k-1) + k*sum(lev-1) # number of parameters
  df <- prod(lev) - 1 - n.params # df
  
  return(c(df, n.params))
}


chigSq <- function(d, theta, pi){
  # Compute Chi^2 and G^2
  #
  # Args:
  #   d: dummy data
  #   theta: the conditional probabilities
  #   pi: class sizes
  #
  # Returns: vector of Ch^2 and G^2
  
  n <- nrow(d)
  observed <- table(d)
  expected <- Expected(pi, theta, n)

  chi <- sum(((observed-expected)^2)/expected)
  
  g <- 2 * sum(observed*log(observed/expected), na.rm=TRUE)
  
  return(c(chi, g))
}

Expected <- function(pi, theta, n=NA){
  # Compute the expected counts
  #
  # Args:
  #    pi: the class sizes
  #    theta: the conditional probabilities
  #    n: sample size
  #
  # Returns a table with expected counts
  
  
  # Loop over classes
  tab <- lapply(1:length(pi), function(class) {
    
    # Get the probabilities of responses given current class
    probs <- lapply(theta, function(x){
      t(t(x[class,]))
    })
    
    # Compute cross-product of the first two items
    p.table <- (probs[[1]] %*% t(probs[[2]]))
    
    # If there are more items, add dimensions
    if(length(probs)>2){
      for(i in 3:length(probs)){
        # Compute the outer products
        p.table <- drop( p.table %o% probs[[i]] )
      }
    }
    
    # Multiply the resulting probabilty table by sample size to get counts
    return(p.table * n * pi[class])
  })
  
  # Sum the expected counts over all classes and return
  return(Reduce("+", tab))
}

aicbic <- function(llik, n.params, n){
  # Compute AIC and BIC
  #
  # Args:
  #    llik: log-likelihood
  #    n.params: number of parameters
  #    n: sample size
  #
  # Returns: AIC and BIC
  
  aic <- -2*llik + 2*n.params
  bic <- -2*llik + log(n)*n.params
  
  c(aic, bic)
}

entropy <- function(p){
  # Compute the Entropy of class separation (Muthen & Muthen, 2006)
  #
  # Args: 
  #    p: probabtilities oa class membership
  #	
  # Returns: entropy of class separation (bounded at <0,1>)
  
  k <- ncol(p)
  if(k<=1){
    warning("Cannot compute entropy for 1 class model")
    return(1)
  }
  
  n <- nrow(p)
  log.p <- log(p)
  log.p[log.p==-Inf] <- 0
  
  ent <- sum(p * log.p)
  ent <- 1 + (ent/(n*log(k)))
  
  # Output
  return(ent)
}

fitMeasures <- function(d, rawd, model){
  # Wrapper for the fit functions
  #
  # Args:
  #    d: dummy data
  #    rawd: raw data
  #    model: the output of the emLCA
  #
  # Output: n of classes, chi^2, G^2, df, n of parameters,
  #         AIC, BIC, Entropy
  
  classes <- model$classes
  df.n.params <- nParamsDf(classes, d)
  chi.g <- chigSq(rawd, model$theta, model$pi)
  aicbic <- aicbic(model$llik, df.n.params[2], nrow(model$posterior))
  entropy <- entropy(model$posterior)
  
  c(classes, round(chi.g, 2), df.n.params,
    round(c(aicbic, entropy), 2))
}

multiFitMeasures <- function(d, rawd, models){
  # Compute the fitmeasures for multiple models
  # 
  # Args:
  #    d: dummy data
  #    rawd: raw data
  #    models: the optimal models
  #
  # Returns a table with the fit measures per model
  
  tab <- sapply(models, function(model){
    fitMeasures(d, rawd, model)
    })
  
  tab <- t(tab)
  colnames(tab) <- c("classes","Chi", "G", "df", "nParams", "AIC", "BIC", "Entropy")
  
  return(tab)
}