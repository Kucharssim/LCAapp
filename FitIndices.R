
entropy <- function(p){
## Compute the Entropy of class separation (Muthén & Muthén, 2006)
#    input: p (n x k matrix of n cases, k indicators)
#				Posterior probabilities of class membership
#    output: entropy of class separation (bounded at <0,1>)
  k <- ncol(p)
  if(k<=1){
    warning("Cannot compute entropy for 1 class model")
    return(1)
    }
  n <- nrow(p)
  #log.p <- log(p)
  
  ent <- sum(p * log(p))
  #ent <- 0
  #for (i in 1:n){
  #  ent <- ent + sum(p[i, ] * log(p[i, ]))
  #}
  ent <- 1 + (1/(n*log(k))) * ent
  return(ent)
}