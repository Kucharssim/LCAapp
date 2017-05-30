plotComparison <- function(comparison){
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
  d <- melt(lapply(theta, function(x){ 
    f <- melt(x)
    f[,2] <- as.character(f[,2])
    f
    })
  )
  d$Var2 <- as.factor(d$Var2)
  colnames(d) <- c("Class", "Level", "Var", "Prob", "Item")
  d$Probability <- paste("Probability:", round(d$Prob, 2))
  
  if(by.item){
    ggplot(data=d, aes(x=Class, y=Prob,
                       fill=Level, text=Probability)) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ Item) +
      ylab("Probability") + xlab("") + guides(fill=FALSE)
  } else{
    ggplot(data=d, aes(x=Item, y=Prob,
                       fill=Level, text=Probability)) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ Class) + 
      ylab("Probability") + xlab("") + guides(fill=FALSE)
  }
}

