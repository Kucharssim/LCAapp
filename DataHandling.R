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


d2 <- data.frame(x1=sample(1:3, 100, TRUE), 
                 x2=sample(1:5, 100, TRUE),
                 x3=sample(c(TRUE, FALSE), 100, TRUE),
                 x4=sample(c("ano", "ne", "neser"), 100, TRUE)
                 )

