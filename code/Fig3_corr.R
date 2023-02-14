

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel("data/corr_data.xlsx")


# Build data
################################################################################

# Order
spp <- c("Yellow perch", "Walleye", "Northern pike", "Smallmouth bass", 
         "Bluegill", "Black crappie", "Largemouth bass")

# Format data
data <- data_orig %>% 
  rename(species1=species) %>% 
  gather(key="species2", value="corr", 2:ncol(.)) %>% 
  mutate(corr=ifelse(corr==1, NA, corr),
         species1=factor(species1, levels=spp),
         species2=factor(species2, levels=spp)) 


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title=element_blank(),
                     legend.text = element_text(size=6),
                     legend.title = element_text(size=6),
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
g <- ggplot(data, aes(x=species1, y=species2, fill=corr)) +
  geom_raster() +
  geom_text(data=data, mapping=aes(x=species1, y=species2, label=corr), size=2.4) +
  # Legend
  scale_fill_gradient2(name="Correlation", midpoint=0, 
                       mid="white", high="navy", low="darkred", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_corr.png"), 
       width=4.5, height=3.5, units="in", dpi=600)





