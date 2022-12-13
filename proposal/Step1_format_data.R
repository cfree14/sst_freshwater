
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
datadir <- "data"
plotdir <- "figures"
codedir <- "code/spmodels"

# Source code
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "ML biomass and harvest 2019.xlsx"))


# Format data
################################################################################

# Function to calculate surplus production: 
# SP(t) = TB(t+1) - TB(t) + C(t)
calc_sp <- function(biomass, catch){
  sp <- c(biomass[2:length(biomass)] - biomass[1:(length(biomass)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}

# Format data
data <- data_orig %>% 
  # Clean name
  janitor::clean_names("snake") %>% 
  rename("temp_c"="mean_surface_temp",
         "ssb_lb"="spawning_stock_biomass",
         "catch_lb"="total_kill_lbs",
         "age3plus_lb"="biomass_age3_plus") %>%
  # Add meta-data
  mutate(comm_name="Walleye",
         species="Sander vitreus",
         lake_name="Mille Lacs Lake, Minnesota",
         stockid=paste(comm_name, lake_name)) %>% 
  # Rearrange
  select(stockid, comm_name, species, lake_name, year, 
         ssb_lb, age3plus_lb, catch_lb, temp_c, ice_out_day, everything()) %>% 
  # Calculate surplus production
  mutate(biomass_lb=age3plus_lb, # age3plus_lb or ssb_lb
         sprod_lb=calc_sp(biomass=biomass_lb, catch = catch_lb)) %>% 
  # Remove missing data
  filter(!is.na(sprod_lb)) %>% 
  # Standardize variables
  mutate(temp_c_sd=scale(temp_c, center=T, scale=F),
         ice_out_day_sd=scale(ice_out_day, center=T, scale=F),
         biomass_lb_sd=biomass_lb/max(biomass_lb),
         sprod_lb_sd=sprod_lb/max(biomass_lb))
  
# Stocks 
stockids <- unique(data$stockid)


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Biomass data for plotting
bdata <- data %>% 
  select(year, ssb_lb, age3plus_lb) %>% 
  gather(key="biomass_type", value="biomass_lb", 2:3) %>% 
  mutate(biomass_type=recode(biomass_type, "ssb_lb"="Spawners",
                             "age3plus_lb"="Age 3+"))

# Plot biomass
g1 <- ggplot(bdata, aes(x=year, y=biomass_lb/1000, color=biomass_type)) +
  geom_line() +
  labs(x="", y="Biomass (1000s lbs)") +
  scale_color_discrete(name="") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.7, 0.9),
        legend.background = element_rect(fill=alpha('blue', 0)))
#g1

# Plot catch
g2 <- ggplot(data, aes(x=year, y=catch_lb/1000)) +
  geom_line() +
  labs(x="", y="Catch (1000s lbs)") +
  theme_bw() + my_theme
#g2

# Plot production
g3 <- ggplot(data, aes(x=year, y=sprod_lb/1000)) +
  geom_line() +
  labs(x="", y="Production (1000s lbs)") +
  theme_bw() + my_theme
#g3

# Plot temperature
g4 <- ggplot(data, aes(x=year, y=temp_c)) +
  geom_line() +
  labs(x="", y="Surface temperature (°C)") +
  theme_bw() + my_theme
#g4

# Plot ice out day
g5 <- ggplot(data, aes(x=year, y=ice_out_day)) +
  geom_line() +
  labs(x="", y="Ice out day") +
  theme_bw() + my_theme
#g5

# Plot ice out day ~ temperature correlation 
g6 <- ggplot(data, aes(x=temp_c, y=ice_out_day)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x="Surface temperature (°C)", y="Ice out day") +
  theme_bw() + my_theme
#g6

# Plot production by temperature
g7 <- ggplot(data, aes(x=age3plus_lb/1000, y=sprod_lb/1000, fill=temp_c_sd)) +
  geom_point(size=3, shape=21) +
  labs(x="Age 3+ biomass (1000s lbs)", y="Production (1000s lbs)") +
  scale_fill_gradient2(name="Temperature\nanamoly (°C)", midpoint=0, low = "navyblue", high="darkred", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
#g7

# Plot production by ice out day
g8 <- ggplot(data, aes(x=age3plus_lb/1000, y=sprod_lb/1000, fill=ice_out_day_sd)) +
  geom_point(size=3, shape=21) +
  labs(x="Age 3+ biomass (1000s lbs)", y="Production (1000s lbs)") +
  scale_fill_gradient2(name="Ice out day\nanamoly", midpoint=0, low = "darkred", high="navyblue", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + my_theme
#g8

# Merge into one plot
layout_matrix <- matrix(data=c(1,1,2,2,3,3,
                               4,4,5,5,6,6,
                               7,7,7,8,8,8), byrow = T, ncol=6)
g <- grid.arrange(g1, g2, g3, 
                  g4, g5, g6, 
                  g7, g8, layout_matrix=layout_matrix)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_data.png"), 
       width=6.5, height=7, units="in", dpi=600)


# Fit models
################################################################################

# Fit production model
output1 <- fit_pella(data=data, 
                    id_col="stockid", 
                    sp_col="sprod_lb_sd", 
                    b_col="biomass_lb_sd", 
                    p=1)

# Format results
results1 <- get_results(output=output1)

# Ice out model
################################

# Fit production model
output2 <- fit_pella_cov_fixed(data=data,
                              id_col="stockid", 
                              sp_col="sprod_lb_sd", 
                              b_col="biomass_lb_sd", 
                              cov_col="ice_out_day_sd", # temp_c_sd or ice_out_day_sd
                              p=1)

# Format results
results2 <- get_results(output=output2)

# Plot results
g2 <- plot_prod_curve(data=data, 
                results=results2, 
                cov_col="ice_out_day_sd", 
                cov_label = "Ice out day\nanamoly",
                cov_vals = seq(-20, 20, 10))
g2


# Temperature
################################

# Fit production model
output3 <- fit_pella_cov_fixed(data=data,
                               id_col="stockid", 
                               sp_col="sprod_lb_sd", 
                               b_col="biomass_lb_sd", 
                               cov_col="temp_c_sd", # temp_c_sd or ice_out_day_sd
                               p=1)

# Format results
results3 <- get_results(output=output3)

# Plot results
g3 <- plot_prod_curve(data=data, 
                results=results3, 
                cov_col="temp_c_sd", 
                cov_label = "Temperature\nanamoly (°C)",
                cov_vals = seq(-1, 1, 0.5))
g3


# Merge
################################

g <- gridExtra::grid.arrange(g3, g2, ncol=2)
g

ggsave(g, filename=file.path(plotdir, "figure_production_curves.png"), 
       width=6.5, height=4, units="in", dpi=600)

