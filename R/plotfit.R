plotfit <-
function(fit, d = "best", int = FALSE, xl = -Inf, xu = Inf, ql = NA, qu = NA, lp = FALSE, ex = NA, sf = 3, ind = TRUE, lpw = 1){

  if(d=="beta" & (min(fit$limits) == -Inf | max(fit$limits) == Inf )){stop("Parameter limits must be finite to fit a beta distribution")}
  if(d=="gamma" & min(fit$limits) < 0 ){stop("Lower parameter limit must be non-negative to fit a gamma distribution")}
  if(d=="lognormal" & min(fit$limits) < 0 ){stop("Lower parameter limit must be non-negative to fit a log normal distribution")}
  if(d=="logt" & min(fit$limits) < 0 ){stop("Lower parameter limit must be non-negative to fit a log t distribution")}
  if(is.na(ql)==F & (ql <0 | ql>1 )){stop("Lower feedback quantile must be between 0 and 1")}
  if(is.na(qu)==F & (qu <0 | qu>1 )){stop("Upper feedback quantile must be between 0 and 1")}
  
  
  if(nrow(fit$vals)>1 & is.na(ex)==T & lp==F){
    if(xl == -Inf & min(fit$limits[,1]) > -Inf){xl <- min(fit$limits[,1]) }
    if(xu == Inf & max(fit$limits[,2]) < Inf){xu <- max(fit$limits[,2]) }
    if(int == FALSE){plotgroup(fit, xl, xu, d, bw = T)}else{
      shinyplotgroup(fit, xl, xu, lpw)
    }
  }
  
  if(nrow(fit$vals)>1 & lp==T){
    if(xl == -Inf & min(fit$limits[,1]) > -Inf){xl <- min(fit$limits[,1]) }
    if(xl == -Inf & min(fit$limits[,1]) == -Inf){
      f1 <- feedback(fit, quantiles=0.01, dist=d)
      xl <- min(f1$expert.quantiles)
    }
    
    if(xu == Inf & max(fit$limits[,2]) < Inf){xu <- max(fit$limits[,2]) }
    
    if(xu == Inf & max(fit$limits[,2]) == Inf){
      f2 <- feedback(fit, quantiles=0.99, dist=d)
      xu <- max(f2$expert.quantiles)
    }
    if(int == FALSE){plotlinearpool(fit, xl, xu, ql, qu , d, ind, lpw)}else{
      shinyplotgroup(fit, xl, xu, lpw)
    }
    
  }
  
  if(nrow(fit$vals)>1 & is.na(ex)==F){
    if(xl == -Inf & fit$limits[ex,1] > -Inf){xl <- fit$limits[ex,1] }
    if(xu == Inf & fit$limits[ex,2] < Inf){xu <- fit$limits[ex,2] }
    if(int == FALSE){plotsingle(fit, d, xl, xu, ql, qu, sf, ex)}else{
      shinyplotsingle(fit, xl, xu, ql, qu, ex)
    }
    
  }
  
  if(nrow(fit$vals)==1){
    if(int == FALSE){plotsingle(fit, d, xl, xu, ql, qu, sf, ex = 1)}else{
      shinyplotsingle(fit, xl, xu, ql, qu, ex = 1)
    }
    
  }	
}
