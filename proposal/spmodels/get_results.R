
# Format fixed effects output
# For testing: output <- output2
get_results <- function(output){
  
  # Format parameter estimates
  results_mat <- summary.sdreport(output$sdreport)
  results_df <- data.frame(stockid=output$stockids, 
                           param=rownames(results_mat),
                           est=results_mat[,1],
                           est_se=results_mat[,2], row.names = NULL,
                           stringsAsFactors = F) %>% 
    # Calculate confidence intervals
    mutate(est_lo=est - est_se*1.96,
           est_hi=est + est_se*1.96)
  
  # Log-transformed results
  results_ln <- results_df %>% 
    filter(grepl("ln_", param)) %>% 
    mutate(param=gsub("ln_", "", param)) %>% 
    mutate_at(.vars=paste0("est", c("", "_se", "_lo", "_hi")), .funs=exp) %>% 
    ungroup()
  
  # Theta results
  results_theta <- results_df %>% 
    filter(param=="BetaT")
  
  # Merge results
  results <- bind_rows(results_ln, results_theta) %>% 
    select(-est_se)
  
  # Return
  return(results)
  
}