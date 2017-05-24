plotComparison <- function(comparison){
  d <- as.data.frame(comparison)
  d <- d %>% 
         select(classes, AIC, BIC) %>%
           melt(id="classes")
  ggplot(data=d, aes(x = classes, y = value, colour = variable)) + 
    geom_line() + geom_point()
}

plotProportions <- function(pi, classes){
  d <- data.frame(classes=1:classes,
                  proportions=pi)
  ggplot(data=d, aes(x=classes, y=proportions)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(0,1))
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

test  <- data.frame(person=c("A", "B", "C", "D", "E"), 
                    value1=c(100,150,120,80,150),     
                    value2=c(25,30,45,30,30) , 
                    value3=c(100,120,150,150,200)) 
