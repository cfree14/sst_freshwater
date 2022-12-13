setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project")
setwd("C:/Users/Luoliang/Desktop/OneDrive/Postdoctor/Project")
library(rstan)
library(dplyr)
SPM_stan = stan_model(file="Code/Stan_code/SPM.stan")
############# LB ###############
# reading input
input_LB = read.csv("Data/SPM_data/LB_WI.csv")
sum(is.na(input_LB$gdd_wtr_5c))
sum(is.na(input_LB$stocking_weight))
sum(is.na(input_LB$tri_harvest))
input_LB$tri_harvest[is.na(input_LB$tri_harvest)] = 0
sum(is.na(input_LB$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_LB = input_LB[input_LB$survey_time_cpue>1,]
length(unique(input_LB$wbic))

# delet lakes with no catch 
input_LB = input_LB[input_LB$survey_time_har>1,]
length(unique(input_LB$wbic))

# number of stocks
Nstocks = length(unique(input_LB$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_LB$wbic)[i]
  data_i = input_LB[input_LB$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
 
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.05,0.5)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_LB.rda")
write.csv(output_quantities,"Output/N_WI_LB_quantities.csv",row.names = F)



############# SB ###############
# reading input
input_SB = read.csv("Data/SPM_data/SB_WI.csv")

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_SB = input_SB[input_SB$survey_time_cpue>1,]
length(unique(input_SB$wbic))

# delet lakes with no catch 
input_SB = input_SB[input_SB$survey_time_har>1,]
length(unique(input_SB$wbic))

# number of stocks
Nstocks = length(unique(input_SB$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_SB$wbic)[i]
  data_i = input_SB[input_SB$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.2,0.8)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_SB.rda")
write.csv(output_quantities,"Output/N_WI_SB_quantities.csv",row.names = F)

############# BC ###############
# reading input
input_BC = read.csv("Data/SPM_data/BC_WI.csv")
sum(is.na(input_BC$gdd_wtr_5c))
sum(is.na(input_BC$stocking_weight))
sum(is.na(input_BC$tri_harvest))
input_BC$tri_harvest[is.na(input_BC$tri_harvest)] = 0
sum(is.na(input_BC$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_BC = input_BC[input_BC$survey_time_cpue>1,]
length(unique(input_BC$wbic))

# delet lakes with no catch 
input_BC = input_BC[input_BC$survey_time_har>1,]
length(unique(input_BC$wbic))

# number of stocks
Nstocks = length(unique(input_BC$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_BC$wbic)[i]
  data_i = input_BC[input_BC$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.2,0.8)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_BC.rda")
write.csv(output_quantities,"Output/N_WI_BC_quantities.csv",row.names = F)



############# WE ###############
# reading input
input_WE = read.csv("Data/SPM_data/WE_WI.csv")
sum(is.na(input_WE$gdd_wtr_5c))
sum(is.na(input_WE$stocking_weight))
sum(is.na(input_WE$tri_harvest))
input_WE$tri_harvest[is.na(input_WE$tri_harvest)] = 0
sum(is.na(input_WE$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_WE = input_WE[input_WE$survey_time_cpue>1,]
length(unique(input_WE$wbic))

# delet lakes with no catch 
input_WE = input_WE[input_WE$survey_time_har>1,]
length(unique(input_WE$wbic))

# number of stocks
Nstocks = length(unique(input_WE$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_WE$wbic)[i]
  data_i = input_WE[input_WE$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.05,0.5)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_WE.rda")
write.csv(output_quantities,"Output/N_WI_WE_quantities.csv",row.names = F)

############# YP ###############
# reading input
input_YP = read.csv("Data/SPM_data/YP_WI.csv")
sum(is.na(input_YP$gdd_wtr_5c))
sum(is.na(input_YP$stocking_weight))
sum(is.na(input_YP$tri_harvest))
input_YP$tri_harvest[is.na(input_YP$tri_harvest)] = 0
sum(is.na(input_YP$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_YP = input_YP[input_YP$survey_time_cpue>1,]
length(unique(input_YP$wbic))

# delet lakes with no catch 
input_YP = input_YP[input_YP$survey_time_har>1,]
length(unique(input_YP$wbic))

# number of stocks
Nstocks = length(unique(input_YP$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_YP$wbic)[i]
  data_i = input_YP[input_YP$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.2,0.8)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_YP.rda")
write.csv(output_quantities,"Output/N_WI_YP_quantities.csv",row.names = F)

############# CC ###############
# reading input
input_CC = read.csv("Data/SPM_data/CC_WI.csv")
sum(is.na(input_CC$gdd_wtr_5c))
sum(is.na(input_CC$stocking_weight))
sum(is.na(input_CC$tri_harvest))
input_CC$tri_harvest[is.na(input_CC$tri_harvest)] = 0
sum(is.na(input_CC$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_CC = input_CC[input_CC$survey_time_cpue>0,]
length(unique(input_CC$wbic))

# delet lakes with no catch 
input_CC = input_CC[input_CC$survey_time_har>0,]
length(unique(input_CC$wbic))

# number of stocks
Nstocks = length(unique(input_CC$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_CC$wbic)[i]
  data_i = input_CC[input_CC$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.05,0.5)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_CC.rda")
write.csv(output_quantities,"Output/N_WI_CC_quantities.csv",row.names = F)

############# NP ###############
# reading input
input_NP = read.csv("Data/SPM_data/NP_WI.csv")
sum(is.na(input_NP$gdd_wtr_5c))
sum(is.na(input_NP$stocking_weight))
sum(is.na(input_NP$tri_harvest))
input_NP$tri_harvest[is.na(input_NP$tri_harvest)] = 0
sum(is.na(input_NP$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_NP = input_NP[input_NP$survey_time_cpue>1,]
length(unique(input_NP$wbic))

# delet lakes with no catch 
input_NP = input_NP[input_NP$survey_time_har>1,]
length(unique(input_NP$wbic))

# number of stocks
Nstocks = length(unique(input_NP$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_NP$wbic)[i]
  data_i = input_NP[input_NP$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.05,0.5)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_NP.rda")
write.csv(output_quantities,"Output/N_WI_NP_quantities.csv",row.names = F)

############# BG ###############
# reading input
input_BG = read.csv("Data/SPM_data/BG_WI.csv")
sum(is.na(input_BG$gdd_wtr_5c))
sum(is.na(input_BG$stocking_weight))
sum(is.na(input_BG$tri_harvest))
input_BG$tri_harvest[is.na(input_BG$tri_harvest)] = 0
sum(is.na(input_BG$cpe))

# select the lakes that have adequate survey times
# delete lakes with only one cpue
input_BG = input_BG[input_BG$survey_time_cpue>1,]
length(unique(input_BG$wbic))

# delet lakes with no catch 
input_BG = input_BG[input_BG$survey_time_har>1,]
length(unique(input_BG$wbic))

# number of stocks
Nstocks = length(unique(input_BG$wbic))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  wbic_i = unique(input_BG$wbic)[i]
  data_i = input_BG[input_BG$wbic==wbic_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$annual_har_w>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$annual_har_w
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.2,0.8)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  
  
}

saveRDS(output,"Output/N_WI_BG.rda")
write.csv(output_quantities,"Output/N_WI_BG_quantities.csv",row.names = F)







