

# Read data
################################################################################

# Clear workspace
rm(list = ls())
options(scipen=999)

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "output"

# Read data
data_orig <- readRDS(file.path(datadir, "output_sim.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Calculate fishing effect
  mutate(effect_fishing=(p_terminal-p_no_fishing)*-1,
         effect_warming=(p_terminal-p_no_env)*-1)

# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=effect_fishing %>% pmin(.,0.5), 
                 y=effect_warming %>% pmin(., 0.5) %>% pmax(., -0.5))) +
  geom_point(fill="grey40", pch=21, alpha=0.6) +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_abline(slope=-1, color="blue") +
  geom_abline(slope=1, color="red") +
  # Label quadrants
  annotate(geom="text", x=0.1, y=0.3, 
           label="Warming worse\nthan fishing", color="darkred") +
  annotate(geom="text", x=0.4, y=0.1, 
           label="Fishing worse\nthan warming", color="red") +
  annotate(geom="text", x=0.1, y=-0.3, 
           label="Warming fully\noffsets fishing", color="blue") +
  annotate(geom="text", x=0.4, y=-0.1, 
           label="Warming partially\noffsets fishing", color="navy") +
  # Labels
  labs(x="Contribution of fishing\nto stock depletion", 
       y="Contribution of warming\nto stock depletion") +
  # Theme
  theme_bw() + base_theme

# Plot
ggsave(g, filename=file.path(plotdir, "FigX_effects_of_fishing_vs_warming.png"), 
       width=4.5, height=4.5, units="in", dpi=600)
