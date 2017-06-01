reshapeData <- function(d){
  # This function takes the raw data and converts it into 
  # a model binary data
  # Args:
  #   d: raw data  
  
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
  # Takes in the data and returns a 
  # vector with number of levels in each variable
  
  if(is.list(d)){
    lev <- sapply(d, ncol)
  } else {
    lev <- ncol(d)
  }
  
  return(lev)
}

whichIdentified <- function(d){
  # Computes the maximum number of classes
  # that is stil identified with the data
  
  lev <- Levels(d)
  k <- prod(lev) / (1+sum(lev-1))
  return(floor(k))
}

exportParameters <- function(pi, theta){
  # Prepares the estimated parameters into a table for export
  #  
  # Args:
  #   pi: Vector of class proportions
  #   theta: A list of conditional probabilities of responses given class
  #  
  # Return:
  #   A table with the parameters
  
  d <- melt(c(pi, theta))

  d[1:length(pi), 2] <- levels(d$Var1)
  d[1:length(pi), 3] <- "Proportions"
  d[1:length(pi), 4] <- paste0("P(", levels(d$Var1), ")")
  
  colnames(d) <- c("Probability", "Class", "Level", "Item")
  return(d[,c(1,2,4,3)])
}