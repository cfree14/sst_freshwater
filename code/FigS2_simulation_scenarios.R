

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

# Exploitation
exp <- tibble(year=0:30,
              f_hi=rnorm(31, mean=0.7, sd=0.1) %>% pmax(., 0),
              f_lo=rnorm(31, mean=0.4, sd=0.1) %>% pmax(., 0),
              sd_hi=rnorm(31, mean=0.5, sd=0.2) %>% pmax(., 0)) %>% 
  gather(key="scenario", value="er", 2:ncol(.)) %>% 
  mutate(scenario=recode_factor(scenario,
                                "f_hi"="High effort, low contrast",
                                "f_lo"="Low effort, low contast",
                                "sd_hi"="High contrast"))

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
g1 <- ggplot(exp, aes(x=year, y=er)) +
  facet_wrap(~scenario, nrow=1) +
  geom_line() +
  # Labels
  labs(x="Year", y="Exploitation rate", tag="A") +
  scale_y_continuous(lim=c(0,1), breaks=seq(0, 1, 0.2)) +
  # Theme
  theme_bw() + base_theme
g1

# Plot data
g2 <- ggplot(exp, aes(x=year, y=er)) +
  facet_wrap(~scenario, nrow=1) +
  geom_line() +
  # Labels
  labs(x="Year", y="Biomass", tag="B") +
  scale_y_continuous(lim=c(0,1), breaks=seq(0, 1, 0.2)) +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=2)
g


# Export plot
ggsave(g, filename=file.path(plotdir, "FigS2_simulation_scenarios.png"), 
       width=6.5, height=5, units="in", dpi=600)





