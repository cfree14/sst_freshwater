
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(TMBhelper)

# Source code
codedir <- "code/spmodels"
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Read data
load("/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/data/data_final_sst.Rdata")
data_orig <- data
rm(data, stocks)


# Format data
################################################################################

# Problem stocks
problem_stocks <- c("ELETERSDB", "SEALIONSCBpup", # production unrelated to abundance
                    "PERPELPERU614S", "PERBOOPERU614S", # enormous influences
                    "COMGUISHETALL", "HUMPBACKCAOR") # enormous influence SDs

# Remove problem stocks
data <- data_orig %>% 
  filter(!stockid %in% problem_stocks)

# Parameters
stocks <- unique(data$stockid)
nstocks <- length(stocks)


# Test functions
################################################################################

# Fit PT no covariate
spfit <- fit_pella(data=data, id_col="stockid", sp_col="sp_sd", b_col="tb_sd", p=1)

# Format results
results <- get_results(spfit)
plot_param_hist(results)

# Fit PT with covariate
spfit <- fit_pella_cov_fixed(data=data, id_col="stockid",
                             sp_col="sp_sd", b_col="tb_sd", cov_col="sst_c_sd", p=1)

# Format results
results <- get_results(spfit)
plot_param_hist(results)



