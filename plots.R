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
    f <- melt(x);
    f[,2] <- as.character(f[,2]);
    f
    })
  )
  
  #d <- melt(theta)
  d$Levels <- as.factor(d$Var2)
  if(by.item){
    ggplot(data=d, aes(x=Var1, y=value, fill=Levels)) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ L1) +
      ylab("Probability")
  } else{
    ggplot(data=d, aes(x=L1, y=value, fill=Levels)) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ Var1) + 
      ylab("Probability")
  }
}

