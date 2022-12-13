

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/raw"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(datadir)

# Merge
data_orig <- purrr::map_df(files2merge, function(x){
  fdata <- read.csv(file.path(datadir, x), as.is = T) %>% 
    mutate_all(as.character) %>% 
    mutate(filename=x)
})

# Format
data <- data_orig %>% 
  # Rename
  rename(lat_dd=centroid_lat,
         long_dd=centroid_lon, 
         lake_name2=LKNAME, 
         gdd_5c=gdd_wtr_5c,
         cpue=cpe,
         harvest_kg_rec=annual_har_w,
         harvest_kg_tribal=tri_harvest,
         stocking_kg=stocking_weight,
         open_water_days=open_water_duration,
         stock_id=wbic) %>% 
  # Convert to numeric
  mutate(across(.cols=c(long_dd, lat_dd, year, gdd_5c, open_water_days, harvest_kg_rec, harvest_kg_tribal, stocking_kg, cpue), .fns=as.numeric)) %>% 
  # Recode NAs
  mutate(cpue=ifelse(cpue==-999, NA, cpue),
         harvest_kg_rec=ifelse(harvest_kg_rec==-999, NA, harvest_kg_rec)) %>% 
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
  select(filename, state, county, lake_name, lake_name2, site_id, long_dd, lat_dd,
         stock_id, species, year,
         open_water_days, gdd_5c,
         stocking_kg, harvest_kg_rec, harvest_kg_tribal,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$state)
table(data$species)

# Sites
sites <- data %>% 
  select(state, county, lake_name, lake_name2, site_id, long_dd, lat_dd) %>% 
  unique()

# Export data
saveRDS(data, file.path("data/data.Rds"))

