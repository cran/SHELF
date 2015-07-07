plinearpool <-
function(fit, x, d = "best", w = 1){
	
  if(min(w)<0 | max(w)<=0){stop("expert weights must be non-negative, and at least one weight must be greater than 0.")}
  
	n.experts <- nrow(fit$vals)
  if(length(w)==1){
    w <- rep(w, n.experts)
  }
  
	px <- matrix(0, length(x), n.experts)
	weight <- matrix(w/sum(w), length(x), n.experts, byrow = T)
	for(i in 1:n.experts){
		px[,i] <- expertprobs(fit, x, d, ex = i)
	}
	
	apply(px * weight, 1, sum)
			
}
