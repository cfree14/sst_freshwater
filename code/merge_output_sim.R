

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "output/raw"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(datadir, pattern=".csv")

# Loop through and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata_orig <- read.csv(file.path(datadir, x))
  
  # Format data
  fdata <- fdata_orig %>% 
    mutate(filename=x,
           stock=1:n())
    
})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Add state and species
  mutate(filename1=filename %>% gsub("_quantities.csv", "", .)) %>% 
  separate(col=filename1, into=c("silly", "state", "species"), sep="_", remove = F) %>% 
  select(-c(filename1, silly)) %>% 
  # Format species
  mutate(species=recode(species,
                        "BC"="Black crappie",    
                        "BG"="Bluegill",  
                        "CC"="Cisco",
                        "LB"="Largemouth bass",
                        "NP"="Northern pike",
                        "SB"="Smallmouth bass",
                        "WE"="Walleye",
                        "YP"="Yellow perch")) %>% 
  # Arrange
  select(filename, state, species, stock, everything())


# Export data
saveRDS(data, file="output/output_sim.Rds")


# Plot parameters
################################################################################

# Plot histograms
g <- ggplot(data, aes(x=est, fill=species)) +
  facet_wrap(~param, scales="free") +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Estimate", y="Number of stocks") +
  scale_fill_discrete(name="Species") +
  # Theme
  theme_bw()
g


# Theta estimates
################################################################################

# Format
thetas <- data %>% 
  # Reduce to thetas
  filter(param=="theta") %>% 
  # Sort
  arrange(species, desc(est)) %>% 
  # Add stock id
  mutate(stock_id=paste(state, species, stock),
         stock_id=factor(stock_id, levels=stock_id))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot theta
g <-  ggplot(thetas, aes(x=est, y=stock_id)) +
  facet_grid(species~., space="free_y", scale="free_y") +
  geom_point() +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Temperature effect", y="Stock") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_blank())
g


# Plot thetas
g <- ggplot(thetas, aes(x=est, y=species)) +
  geom_violin() +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Temperature effect", y="") +
  # Theme
  theme_bw()
g





