nParamsDf <- function(k, tab.d){

  lev <- Levels(tab.d)
  
  n.params <- (k-1) + k*sum(lev-1)
  df <- prod(lev) - 1 - n.params
  
  return(c(df, n.params))
}


chigSq <- function(d, theta, pi){
  n <- nrow(d)
  observed <- table(d)
  expected <- Expected(pi, theta, n)

  chi <- sum(((observed-expected)^2)/expected)
  
  g <- 2 * sum(observed*log(observed/expected), na.rm=TRUE)
  
  return(c(chi, g))
}

Expected <- function(pi, theta, n=NA){
  
  tab <- lapply(1:length(pi), function(class) {
    probs <- lapply(theta, function(x){
      t(t(x[class,]))
    })
    
    p.table <- (probs[[1]] %*% t(probs[[2]]))
    
    if(length(probs)>2){
      for(i in 3:length(probs)){
        p.table <- drop( p.table %o% probs[[i]] )
      }
    }
    
    return(p.table * n * pi[class])
  })
  
  return(Reduce("+", tab))
}

# weightProb <- function(pi, theta){
#   # Weights the probabilities of answers under different classes by their probability
#   # input: pi - vector of class probabilities
#   #        theta - list of conditional probs
#   # output: list of vectors of answering patterns per item
#   
#   wP <- lapply(theta, function(item){
#     t(item) %*% pi
#   })
#   
#   return(wP)
# }

aicbic <- function(llik, nParams, n){
  aic <- -2*llik + 2*nParams
  bic <- -2*llik + log(n)*nParams
  c(aic, bic)
}

entropy <- function(p){
## Compute the Entropy of class separation (Muth?n & Muth?n, 2006)
#    input: p (n x k matrix of n cases, k indicators)
#				Posterior probabilities of class membership
#    output: entropy of class separation (bounded at <0,1>)
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
  return(ent)
}

fitMeasures <- function(d, rawd, model){
  classes <- model$classes
  df.n.params <- nParamsDf(classes, d)
  chi.g <- chigSq(rawd, model$theta, model$pi)
  aicbic <- aicbic(model$llik, df.n.params[2], nrow(model$posterior))
  entropy <- entropy(model$posterior)
  
  c(classes, round(chi.g, 2), df.n.params,
    round(c(aicbic, entropy), 2))
}

multiFitMeasures <- function(d, rawd, models){
  #clusterExport(cl=cl, varlist = funLCA)
  tab <- sapply(models, function(model){
    fitMeasures(d, rawd, model)
    })
  tab <- t(tab)
  colnames(tab) <- c("classes","Chi", "G", "df", "nParams", "AIC", "BIC", "Entropy")
  return(tab)
}