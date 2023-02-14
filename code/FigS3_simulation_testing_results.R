

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Simulate data
################################################################################

# Build data
combos <- expand.grid(data_quantity=paste0("D", 1:5),
                      effort_scenario=paste0("F", 1:3)) %>% 
  mutate(data_quantity=recode_factor(data_quantity,
                                    "D1"="1 harvest, 2 CPUE", 
                                    "D2"="2 harvest, 4 CPUE",
                                    "D3"="3 harvest, 6 CPUE",
                                    "D4"="4 harvest, 8 CPUE", 
                                    "D5"="5 harvest, 10 CPUE"),
         effort_scenario=recode_factor(effort_scenario,
                                      "F1"="High effort, low contrast",
                                      "F2"="Low effort, low contrast",
                                      "F3"="High contrast"))

# Simulate data
ndo <- 100
data <- purrr::map_df(1:nrow(combos), function(x){
  true <- runif(ndo, -1.7, 1.7)
  est <- true+rnorm(ndo, 0, 0.3)
  out <- tibble(data_quantity=combos$data_quantity[x],
                effort_scenario=combos$effort_scenario[x],
                true=true, est=est)
})

# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     strip.text = element_text(size=7),
                     plot.tag = element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=true, y=est)) +
  facet_grid(effort_scenario ~ data_quantity) +
  # Reference line
  geom_hline(yintercept=0, linetype="dotted", color="grey60") +
  geom_vline(xintercept=0, linetype="dotted", color="grey60") +
  geom_abline(slope=1, intercept=0) +
  # Points
  geom_point(pch=21) +
  # Labels
  labs(x="True warming impact", y="Estimated warming impact") +
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5)) +
  scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5)) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS3_simulation_testing_results.png"), 
       width=6.5, height=5, units="in", dpi=600)





