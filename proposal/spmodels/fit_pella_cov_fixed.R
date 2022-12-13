

# Fit surplus production model
fit_pella_cov_fixed <- function(data, id_col, sp_col, b_col, cov_col, p){

  # Parameters
  stockids <- data %>% pull(id_col) %>% unique()
  nstocks <- length(stockids)
  bmax <- data %>% pull(b_col) %>% max()
  
  # Compile TMB code
  # Only run once to compile code
  if(FALSE){
    dyn.unload(paste(tmbdir, dynlib("pella_prey_fixed"), sep="/"))
    file.remove(paste(tmbdir, c("pella_prey_fixed.o", "pella_prey_fixed.dll"), sep="/"))
    compile("pella_prey_fixed.cpp")
  }
  
  # Load TMB code
  dyn.load(file.path(getwd(), "code/spmodels/tmb_code", dynlib("pella_prey_fixed")) )

  # Input data and parameter starting values
  params <- list(ln_B0=rep(log(1.5*bmax), nstocks),
                 ln_r=rep(log(0.4), nstocks),
                 BetaT=rep(0.0, nstocks),
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data %>% pull(id_col)),
                     B_t=data %>% pull(b_col),
                     P_t=data %>% pull(sp_col),
                     Prey_t=data %>% pull(cov_col))
  
  # Initialization
  model <- MakeADFun(data=input.data, parameters=params, DLL="pella_prey_fixed")
  # model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
  # model$hessian <- F
  # newtonOption(model, smartsearch=TRUE)
  
  # Run model
  output <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, 
                               newtonsteps=3, bias.correct=FALSE, getsd=FALSE)
  
  
  # 3. Check fit
  ######################################
  
  # Use hessian to diagnose fixed effects that might cause a problem
  hess <- optimHess(par=output$par, fn=model$fn, gr=model$gr)
  problem.vals <- which(eigen(hess)$values<0)
  if(length(problem.vals)>0 ){
    display <- eigen(hess)$vectors[,problem.vals]
    names(display) = (output$diagnostics$Param)
    cbind(1:length(output$par), output$par, display)
  }
  
  # Calculate SD
  sd <- try(sdreport(model, hessian.fixed=hess))
  
  # Return model fit
  output <- list(data=data, tmbfit=output, sdreport=sd, stockids=stockids)
  return(output)
  
}
