reshapeData <- function(d){
## This function takes the raw data and converts it into 
## a model binary data
    
  
  # Loop over items
  tab.d <- lapply(1:ncol(d), function(item){
    
    f.item <- factor(d[,item]) # create a factor vector
    m <- model.matrix(~ f.item - 1) # turn it into a dummy matrix 
    colnames(m) <- levels(f.item) # name the columns
    m
  })
  
  # name the items
  names(tab.d) <- colnames(d) 
  return(tab.d)
}

Levels <- function(d){
  if(is.list(d)){
    lev <- sapply(d, ncol)
  } else {
    lev <- ncol(d)
  }
  
  return(lev)
}

whichIdentified <- function(d){
  lev <- Levels(d)
  k <- prod(lev) / (1+sum(lev-1))
  return(floor(k))
}