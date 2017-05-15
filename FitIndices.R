df <- function(k, tab.d){
  m <- sapply(tab.d, function(item){
    ncol(item)
  })
  
  #if(k>1){
    return((k-1) + k*sum(m-1))
  #} else(return(0))
}

chisq <- function(d, theta, pi){
  #observed <- table(d)
  n <- nrow(d)
  observed <- apply(d, 2, table)
  # expected <- sapply(theta, function(item){
  #   t(pi) %*% item
  # })
  
  expected <- numeric()
  for ( )
  expected <- expected * n
  chi <- sum(((observed-expected)^2)/expected)
  
  list(observed, expected, chi)
}

aicbic <- function(llik, df, n){
  aic <- -2*llik + 2*df
  bic <- -2*llik + log(n)*df
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

fitMeasures <- function(d, model){
  #print(model$classes)
  classes <- model$classes
  df <- df(classes, d)
  aicbic <- aicbic(model$llik, df, nrow(model$posterior))
  entropy <- entropy(model$posterior)
  
  c(classes, df, aicbic, entropy)
}

multiFitMeasures <- function(d, models){
  tab <- sapply(models, function(model){
    #print(model)
    fitMeasures(d, model)
    })
  tab <- t(tab)
  colnames(tab) <- c("classes", "df", "AIC", "BIC", "Entropy")
  return(tab)
}