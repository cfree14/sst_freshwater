

# Plot production curve
# For testing: 
# results <- results3; cov_vals <- seq(-1,1,0.5)
# cov_col <- "temp_c_sd"
# cov_label <- "Temperature\nanamoly (°C)"
plot_prod_curve <- function(data, results, cov_col, cov_label, cov_vals){
  
  # Setup theme
  my_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=10),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=10),
                    plot.title=element_text(size=12),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"), 
                    legend.position="bottom")
  
  # Extract parameters
  p <- 1
  r <- results %>% filter(param=="r") %>% pull(est)
  b0 <- results %>% filter(param=="B0") %>% pull(est)
  theta <- results %>% filter(param=="BetaT") %>% pull(est)
  
  # Make predictions
  biomass_vals <- seq(0, 1, 0.01)
  preds_df <- purrr::map_df(cov_vals, function(x) {
    # Make predictions
    sp_preds <- r/p * biomass_vals * (1-(biomass_vals/b0)^p) * exp(x * theta)
    # Build dataframe
    df <- data.frame(cov=x,
                     biomass=biomass_vals,
                     sp=sp_preds)
  })
  
  # Plot data
  g <- ggplot(data, aes(x=biomass_lb_sd, y=sprod_lb_sd, fill=get(cov_col))) +
    # Plot points
    geom_point(size=3, shape=21) +
    # Plot production lines
    geom_line(data=preds_df, mapping=aes(x=biomass, y=sp, 
                                         group=cov, color=cov, fill=NULL)) +
    geom_line(data=preds_df %>% filter(cov==0),
              mapping=aes(x=biomass, y=sp, fill=NULL), color="black") +
    # Labels
    labs(x="Biomass, standardized", y="Production, standardized") +
    # Point legend
    scale_fill_gradient2(name=cov_label, 
                         midpoint=0, low = "navyblue", high="darkred", mid="white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Prediction line legend
    scale_color_gradient2(name=cov_label, 
                          midpoint=0, low = "navyblue", high="darkred", mid="white", guide=F) +
    # Theme
    theme_bw() + my_theme 
  g
  
  
}