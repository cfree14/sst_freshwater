

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
ndo <- 100
data <- tibble(scenario=c(rep("Fox", ndo), rep("Schaefer", ndo), rep("Priors", ndo)),
               base=runif(ndo*3, -1.7, 1.7)) %>% 
  mutate(alternate=base+rnorm(ndo*3, 0, 0.05),
         scenario=recode_factor(scenario,
                                "Schaefer"="Schaefer model\nparameterization",
                                "Fox"="Fox model\nparameterization",
                                "Priors"="Wider prior\nspecification"))

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
g <- ggplot(data, aes(x=base, y=alternate)) +
  facet_wrap(~scenario, nrow=1) +
  # Reference line
  geom_hline(yintercept=0, linetype="dotted", color="grey60") +
  geom_vline(xintercept=0, linetype="dotted", color="grey60") +
  geom_abline(slope=1, intercept=0) +
  # Points
  geom_point(pch=21) +
  # Labels
  labs(x="Warming impact\nestimated by the base model", y="Warming impact\nestimated by an alternative model") +
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5)) +
  scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5)) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_sensitivity_models.png"), 
       width=6.5, height=2.5, units="in", dpi=600)





