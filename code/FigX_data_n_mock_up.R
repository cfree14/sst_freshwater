

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Buidl data
data <- expand.grid(catch_n=1:10,
                    cpue_n=1:10) %>% 
  mutate(n=runif(100,0,100))

# Plot data
g <- ggplot(data, aes(x=cpue_n, y=catch_n, fill=n)) +
  geom_raster() +
  # Labels
  labs(x="Number of years of CPUE data",
       y="Number of years of catch data") +
  # Legend
  scale_fill_gradientn(name="Number of populations",
                        colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g
