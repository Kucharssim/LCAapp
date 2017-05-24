plotComparison <- function(comparison){
  d <- as.data.frame(comparison)
  d <- d %>% 
         select(classes, AIC, BIC) %>%
           melt(id="classes")
  ggplot(data=d, aes(x = classes, y = value, colour = variable)) + 
    geom_line() + geom_point()
}