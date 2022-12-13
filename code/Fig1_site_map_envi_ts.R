

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada"), returnclass = "sf")

# Read data
data <- readRDS("data/data.Rds")

# Sites
sites <- data %>% 
  filter(!is.na(long_dd) & !is.na(lat_dd)) %>% 
  select(state, county, lake_name, lake_name2, site_id, long_dd, lat_dd) %>% 
  unique()


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.title=element_text(size=7),
                     strip.text = element_text(size=6),
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
g1 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot points
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd)) +
  # Labels
  labs(x="", y=" ", tag="A") +
  # Crop
  coord_sf(xlim = c(-97, -83), ylim = c(37, 49)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank())
g1

# Air temperature
g2 <- ggplot(data, aes(x=year, y=gdd_5c, group=year)) +
  geom_boxplot() +
  # Labels
  labs(x="Year", y="Air temperature (°C)", tag="B") +
  # Theme
  theme_bw() + base_theme
g2

# Growing degree days
g3 <- ggplot(data, aes(x=year, y=gdd_5c, group=year)) +
  geom_boxplot() +
  # Labels
  labs(x="Year", y="Growing degree days", tag="C") +
  # Theme
  theme_bw() + base_theme
g3

# Open water duration
g4 <- ggplot(data, aes(x=year, y=open_water_days, group=year)) +
  geom_boxplot() +
  # Labels
  labs(x="Year", y="Open water duration", tag="D") +
  # Theme
  theme_bw() + base_theme
g4


# CPUE over time
g5 <- ggplot(data, aes(x=year, y=cpue, group=year)) +
  facet_wrap(~species, nrow=1, scales="free_y") +
  geom_boxplot() +
  # Labels
  labs(x="Year", y="CPUE", tag="E") +
  # Theme
  theme_bw() + base_theme + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g5

# Arrange
layout_matrix <- matrix(c(1,2,
                          1,3,
                          1,4,
                          5,5), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, 
                             layout_matrix=layout_matrix, 
                             widths=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_site_map_plus.png"), 
       width=6.5, height=6.5, units="in", dpi=600)





