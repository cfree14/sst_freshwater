

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "output"

# Read data
data_orig <- readRDS(file.path(datadir, "output.Rds"))

# Check completeness (should be 100%)
freeR::complete(data_orig)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Add long parameter name
  mutate(param_long=recode_factor(param,
                                  "p_init"="Initial depletion (B/K)",
                                  "r"="Intrinsic growth rate, r",
                                  "m"="Shape parameter, m",
                                  "theta"="Temperature effect, θ",
                                  "k"="Carrying capacity, K",
                                  "q"="Catchability, q",
                                  "sigma"="CPUE observation error, σ",
                                  "tau"="Catch observation error, τ"))

# Format
thetas <- data %>% 
  # Reduce to thetas
  filter(param=="theta") %>% 
  # Sort
  arrange(species, desc(est)) %>% 
  # Add stock id
  mutate(stock_id=paste(state, species, stock),
         stock_id=factor(stock_id, levels=stock_id))

# Theta stats
theta_stats <- thetas %>% 
  group_by(species) %>% 
  summarise(theta=median(est, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(theta))

# Order thetas
thetas_ordered <- thetas %>% 
  mutate(species=factor(species, levels=theta_stats$species))

# Parameter histogram
################################################################################

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

# Plot histograms
g <- ggplot(data, aes(x=est, fill=species)) +
  facet_wrap(~param_long, scales="free", ncol=4) +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Estimate", y="Density") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank(),
        legend.position="top",
        legend.margin = margin(rep(0,4)),
        legend.key.size = unit(0.4, "cm"))
g

# Plot
ggsave(g, filename=file.path(plotdir, "FigX_parameter_histograms.png"), 
       width=6.5, height=4, units="in", dpi=600)


# Theta estimates
################################################################################

# Species
species1 <- c("Black crappie", "Bluegill", "Cisco", "Largemouth bass")

# Plot theta
g1 <-  ggplot(thetas %>% filter(species %in% species1), aes(x=est, y=stock_id)) +
  facet_grid(species~., space="free_y", scale="free_y") +
  geom_linerange(mapping=aes(y=stock_id, xmin=est_lo, xmax=est_hi), color="grey80") +
  geom_point() +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Temperature effect", y="Stock") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_blank())
g1

# Plot theta
g2 <-  ggplot(thetas %>% filter(!species %in% species1), aes(x=est, y=stock_id)) +
  facet_grid(species~., space="free_y", scale="free_y") +
  geom_linerange(mapping=aes(y=stock_id, xmin=est_lo, xmax=est_hi), color="grey80") +
  geom_point() +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Temperature effect", y="Stock") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1,g2, ncol=2)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_theta_orca_plots.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Theta boxplots
################################################################################

# Species colors
species <- c("Black crappie", "Bluegill", "Cisco", "Largemouth bass", "Northern pike",
             "Smallmouth bass", "Walleye", "Yellowperch")
colors <- c("slategray4", "blue", "pink", "orange", "brown", "green", "navy", "yellow")

# Plot thetas
g <- ggplot(thetas_ordered, aes(x=est, y=species, fill=species)) +
  geom_violin(draw_quantiles = 0.5) +
  # Reference line
  geom_vline(xintercept=0, linetype="dashed") +
  # Labels
  labs(x="Temperature effect", y="") +
  scale_x_continuous(breaks=seq(-3,3,1)) +
  # Legend
  scale_fill_discrete(name="Species") + # values=colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_theta_boxplots.png"), 
       width=6.5, height=3, units="in", dpi=600)




