
plot_param_hist <- function(results){
  
  g <- ggplot(results, aes(x=est)) + 
    facet_wrap(~param, scale="free") +
    geom_histogram() +
    labs(x="Estimate", y="Number of stocks") +
    theme_bw()
  print(g)
  
}
