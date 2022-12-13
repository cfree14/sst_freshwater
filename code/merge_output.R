

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

# Parameters
params <- c("r", "k", "q", "p_init", "sigma", "tau", "m", "theta")
metrics <- c("est", "se", "sd", "est_lo", "est_hi", "rhat", "n_eff")

# Files to merge
files2merge <- list.files(datadir, pattern=".rda")

# Loop through and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata_orig <- readRDS(file.path(datadir, x))
  
  # Format
  # X stocks (rows)
  # 8 parameters (columns): r, k, q, P_initial, sigma, tau, m, theta
  # 7 metrics (layers): mean, se_mean, sd, 2.5, 97.5, Rhat, n_eff

  # Flatten data
  nlayers <- dim(fdata_orig)[3]
  y <- 1
  fdata <- purrr::map_df(1:nlayers, function(y){
    
    # Layer data
    ldata <- fdata_orig[,,y] %>% 
      as.data.frame() %>% 
      set_names(params) %>% 
      mutate(stock=1:n(),
             metric=metrics[y], 
             filename=x) 
    
  })
    
})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Arrange
  select(filename, stock, metric, everything()) %>% 
  # Gather
  gather(key="param", value="value", 4:ncol(.)) %>% 
  # Spread
  spread(key="metric", value="value") %>% 
  # Add state and species
  mutate(filename1=filename %>% gsub(".rda", "", .)) %>% 
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
  select(filename, state, species, stock, param, est, est_lo, est_hi, se, sd, rhat, n_eff, everything())


# Export data
saveRDS(data, file="output/output.Rds")


