plotComparison <- function(comparison){
  # Plot the AIC/BIC for the models
  #  
  # Args: 
  #   comparison: the table of model comparison
  #  
  # Returns a ggplot object of the plot
  
  d <- as.data.frame(comparison)
  d <- d %>% 
         select(classes, AIC, BIC) %>%
           melt(id="classes")
  
  ggplot(data=d, aes(x = factor(classes), y = value,
                     group=variable, colour = variable)) + 
    geom_line() + geom_point(size=3) +
    ylab("") + xlab("") + ggtitle("AIC/BIC for respective models")
}

plotProportions <- function(pi){
  # Plot the class sizes 
  #
  # Args: 
  #   pi: vector of class sizes  
  #
  # Returns a ggplot object of the barplot
  
  d <- data.frame(classes=paste("Class", seq_along(pi)),
                  proportions=pi)
  
  ggplot(data=d, aes(x=factor(classes), y=proportions, fill=proportions)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(0,1)) + 
    ylab("") + xlab("") + ggtitle("Proportions of the class sizes") +
    guides(fill=FALSE)  + 
    coord_fixed(ratio=2)
}

plotProbabilities <- function(theta, by.item=FALSE){
  # Plot the conditional probabilities of responses given class
  #  
  # Args:
  #   theta: list of conditional probabilities
  #   by.item: plot items side by side?
  #  
  # Returns a ggplot object of the stacked barplots with facetting
  
  # prepare data
  d <- melt(lapply(theta, function(x){ 
    f <- melt(x)
    f[,2] <- as.character(f[,2])
    f
    })
  )
  
  d$Var2 <- as.factor(d$Var2)
  colnames(d) <- c("Class", "Level", "Var", "Prob", "Item")
  d$Probability <- paste("Probability:", round(d$Prob, 2))
  
  
  if(by.item){ # plot by items
    ggplot(data=d, aes(x=Class, y=Prob,
                       fill=Level, text=Probability)) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ Item) +
      ylab("Probability") + xlab("") + guides(fill=FALSE)
  } else{ # plot by classes
    ggplot(data=d, aes(x=Item, y=Prob,
                       fill=Level, text=Probability)) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ Class) + 
      ylab("Probability") + xlab("") + guides(fill=FALSE)
  }
}

