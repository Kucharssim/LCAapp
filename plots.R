plotComparison <- function(comparison){
  d <- as.data.frame(comparison)
  d <- d %>% 
         select(classes, AIC, BIC) %>%
           melt(id="classes")
  ggplot(data=d, aes(x = classes, y = value, colour = variable)) + 
    geom_line() + geom_point()
}

plotProportions <- function(pi){
  d <- data.frame(classes=paste("Class", seq_along(pi)),
                  proportions=pi)
  
  ggplot(data=d, aes(x=factor(classes), y=proportions, fill=proportions)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(0,1)) + 
    ylab("") + xlab("") + ggtitle("Proportions of the class sizes") +
    guides(fill=FALSE)
}

plotProbabilities <- function(theta, by.item=FALSE){
  d <- melt(theta)
  if(by.item){
    ggplot(data=d, aes(x=Var1, y=value, fill=as.factor(Var2))) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ L1)
  } else{
    ggplot(data=d, aes(x=L1, y=value, fill=as.factor(Var2))) + 
      geom_bar(stat="identity", position = "stack") + facet_grid(~ Var1)
  }
}

