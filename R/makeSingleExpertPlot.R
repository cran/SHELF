makeSingleExpertPlot <-
function(fit, d = "best", pl = -Inf, pu = Inf,
         ql = NA, qu = NA, sf = 3, ex = 1,
         lwd = 1, xlab, ylab, percentages ){
  
  
  
	if(d == "best"){
	  d <- fit$best.fitting[ex, 1]
	}

	
	if(d == "normal"){
		
		if(pl == -Inf){pl <- qnorm(0.001, fit$Normal[ex,1], fit$Normal[ex,2])}
		if(pu == Inf){pu <- qnorm(0.999, fit$Normal[ex,1], fit$Normal[ex,2])}
		x <- seq(from = pl, to = pu, length = 200)
		if(is.na(ql) == F){
		  x.q1 <- qnorm(ql, fit$Normal[ex,1], fit$Normal[ex,2])
		  x <- sort(c(x, x.q1))
		}
		if(is.na(qu) == F){
		  x.q2 <- qnorm(qu, fit$Normal[ex,1], fit$Normal[ex,2])
		  x <- sort(c(x, x.q2))
		}
		fx <- dnorm(x, fit$Normal[ex,1], fit$Normal[ex,2]) 
		dist.title <- paste("Normal (mean = ",
		                         signif(fit$Normal[ex,1], sf),
		                         ", sd = ",
		                         signif(fit$Normal[ex,2], sf), ")",
		                         sep="")
	}
	
	if(d == "t"){
		
		if(pl == -Inf){pl <- fit$Student.t[ex,1] + fit$Student.t[ex,2] * qt(0.001, fit$Student.t[ex,3])}
		if(pu == Inf){pu <- fit$Student.t[ex,1] + fit$Student.t[ex,2] * qt(0.999, fit$Student.t[ex,3])}
		
		x <- seq(from = pl, to = pu, length = 200)
		
		if(is.na(ql) == F){
		  x.q1 <- fit$Student.t[ex,1] + fit$Student.t[ex,2] * qt(ql, fit$Student.t[ex,3])
		  x <- sort(c(x, x.q1))
		}
		if(is.na(qu) == F){
		  x.q2 <- fit$Student.t[ex,1] + fit$Student.t[ex,2] * qt(qu, fit$Student.t[ex,3])
		  x <- sort(c(x, x.q2))
		} 
		  
		fx <- dt((x - fit$Student.t[ex,1])/fit$Student.t[ex,2], fit$Student.t[ex,3])/fit$Student.t[ex,2]
		
		dist.title=paste("Student-t(",
		                 signif(fit$Student.t[ex,1], sf),
		                 ", ",
		                 signif(fit$Student.t[ex,2], sf),
		                 "), df = ",
		                 fit$Student.t[ex, 3],
		                 sep="")
	}
	
	if(d == "gamma"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		
		if(pl == -Inf){pl <- xl + qgamma(0.001, fit$Gamma[ex,1], fit$Gamma[ex,2])}
		if(pu == Inf){pu <- xl + qgamma(0.999, fit$Gamma[ex,1], fit$Gamma[ex,2])}
		x <- seq(from = pl, to = pu, length = 200)
		
		if(is.na(ql) == F){
		  x.q1 <- xl + qgamma(ql, fit$Gamma[ex,1], fit$Gamma[ex,2])
		  x <- sort(c(x, x.q1))
		}
		
		if(is.na(qu) == F){
		  x.q2 <- xl + qgamma(qu, fit$Gamma[ex,1], fit$Gamma[ex,2])
		  x <- sort(c(x, x.q2))
		}
		
		fx <- dgamma(x - xl, fit$Gamma[ex,1], fit$Gamma[ex,2])  
		
		if(fit$Gamma[ex,1] == 1){
		dist.title = paste("Gamma(",
		                   signif(fit$Gamma[ex,1], sf),
		                   ", ",
		                   signif(fit$Gamma[ex,2], sf),
		                   ") (exponential)", sep="")}else{
		                     dist.title = paste("Gamma(",
		                                        signif(fit$Gamma[ex,1], sf),
		                                        ", ",
		                                        signif(fit$Gamma[ex,2], sf),
		                                        ")", sep="")
		                     
		                   }
	}
	
	if(d == "lognormal"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		
		if(pl == -Inf){pl <- xl + qlnorm(0.001, fit$Log.normal[ex,1], fit$Log.normal[ex,2])}
		if(pu == Inf){pu <- xl + qlnorm(0.999, fit$Log.normal[ex,1], fit$Log.normal[ex,2])}
		x <- seq(from = pl, to = pu, length = 200)
		if(is.na(ql) == F){
		  x.q1 <- xl + qlnorm(ql, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		  x <- sort(c(x, x.q1))}
		if(is.na(qu) == F){
		  x.q2 <- xl + qlnorm(qu, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		  x <- sort(c(x, x.q2))}
		  
		fx <- dlnorm(x - xl, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		
		dist.title = paste("Log normal(",
		                   signif(fit$Log.normal[ex,1], sf),
		                   ", ",
		                   signif(fit$Log.normal[ex,2], sf), ")",
		                   sep="")
	}	
	
	if(d == "logt"){ # log student t
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		
    # Calculate axes limits using the lognormal; log-t limits may be too extreme
		if(pl == -Inf){pl <- xl + qlnorm(0.001, fit$Log.normal[ex,1], fit$Log.normal[ex,2])}
		if(pu == Inf){pu <- xl + qlnorm(0.999, fit$Log.normal[ex,1], fit$Log.normal[ex,2])}
    
		x <- seq(from = pl, to = pu, length = 200)
		if(is.na(ql) == F){
		  x.q1 <- xl + exp(fit$Log.Student.t[ex,1] + fit$Log.Student.t[ex,2] * qt(ql, fit$Log.Student.t[ex,3]))
		  x <- sort(c(x, x.q1))}
		
		if(is.na(qu) == F){
		  x.q2 <- xl + exp(fit$Log.Student.t[ex,1] + fit$Log.Student.t[ex,2] * qt(qu, fit$Log.Student.t[ex,3]))
		  x <- sort(c(x, x.q2))}
		
		fx <- dt( (log(x - xl) - fit$Log.Student.t[ex,1]) / fit$Log.Student.t[ex,2], fit$Log.Student.t[ex,3]) / ((x - xl) * fit$Log.Student.t[ex,2])
		dist.title = paste("Log T(",
		                   signif(fit$Log.Student.t[ex,1], sf),
		                   ", ",
		                   signif(fit$Log.Student.t[ex,2], sf),
		                   "), df = ",
		                   fit$Log.Student.t[ex,3], sep="")

	}	
	
	if(d == "beta"){
		xl <- fit$limits[ex,1]
		xu <- fit$limits[ex,2]
		#if(xl == -Inf){xl <- 0}
		#if(xu == Inf){xu <- 1}
		
	#	if(pl == -Inf){pl <- xl + (xu - xl) * qbeta(0.001, fit$Beta[ex,1], fit$Beta[ex,2])}
	#	if(pu == Inf){pu <- xl + (xu - xl) * qbeta(0.999, fit$Beta[ex,1], fit$Beta[ex,2])}
		if(pl == -Inf){pl <- xl}
		if(pu == Inf){pu <- xu}
			x <-  seq(from = pl, to = pu, length = 200)
		if(is.na(ql) == F){
		  x.q1 <- xl + (xu - xl) * qbeta(ql, fit$Beta[ex,1], fit$Beta[ex,2])
		  x <- sort(c(x, x.q1))}
		
		if(is.na(qu) == F){
		  x.q2 <- xl + (xu - xl) * qbeta(qu, fit$Beta[ex,1], fit$Beta[ex,2])
		  x <- sort(c(x, x.q2))}
		
		fx <-  1/(xu - xl) * dbeta( (x - xl) / (xu - xl), fit$Beta[ex,1], fit$Beta[ex,2])
		
		dist.title =paste("Beta(",
		                        signif(fit$Beta[ex,1], sf),
		                        ", ", signif(fit$Beta[ex,2], sf),
		                        ")", sep="")
	}
	
	if(d == "hist"){
	  
	  if(fit$limits[ex, 1] == -Inf){
	     histl <- qnorm(0.001, fit$Normal[ex,1], fit$Normal[ex,2])
	  }else{
	    histl <- fit$limits[ex, 1]
	  }
	  
	  if(fit$limits[ex, 2] == Inf){
	    histu <- qnorm(0.999, fit$Normal[ex,1], fit$Normal[ex,2])
	  }else{
	    histu <- fit$limits[ex, 2]
	  }
	  
	  
	  if(pl == -Inf){pl <- histl }
	  if(pu == Inf){pu <- histu }
	   
    p <- c(0, fit$probs[ex,], 1)
    x2 <- c(histl, fit$vals[ex,], histu)
    
    h <- rep(0, length(x2) -1)
    for(i in 1:length(h)){
      h[i]<-(p[i+1] - p[i]) / (x2[i+1]-x2[i])
    }
    
    x <- rep(x2, each = 2)
    fx <- c(0, rep(h, each = 2), 0)
    
    if(is.na(ql) == F){
      x.q1 <- qhist(ql, x2, p)
      if(!is.element(x.q1, x)){
      x <- c(x, x.q1)
      fx <-c(fx, dhist(x.q1, x2, p))
      temp <- sort(x, index.return = T)
      x <- temp$x
      fx <- fx[temp$ix]}}
      
    
    if(is.na(qu) == F){
      x.q2 <- qhist(qu, x2, p)
      if(!is.element(x.q2, x)){
      x <- c(x, x.q2)
      fx <-c(fx, dhist(x.q2, x2, p))
      temp <- sort(x, index.return = T)
      x <- temp$x
      fx <- fx[temp$ix]}}
    
    
    
	  if(min(fx)<0){
	    fx <- rep(0, length(x))
	    ql <- NA
	    qu <- NA
	  }
    
    dist.title = "histogram fit"
   
	}
	
	if(d == "mirrorgamma"){
	  xu <- fit$limits[ex, 2]
	  if(pl == -Inf){pl <- xu - qgamma(0.999, fit$mirrorgamma[ex,1],
	                                   fit$mirrorgamma[ex,2])}
	  if(pu == Inf){pu <- xu}
	  
	  x <- seq(from = pl, to = pu, length = 200)
	  
	  if(is.na(ql) == F){
	    x.q1 <- xu - qgamma(1 - ql, fit$mirrorgamma[ex,1],
	                        fit$mirrorgamma[ex,2])
	    x <- sort(c(x, x.q1))
	  }
	  
	  if(is.na(qu) == F){
	    x.q2 <- xu - qgamma(1 - qu, fit$mirrorgamma[ex,1],
	                        fit$mirrorgamma[ex,2])
	    x <- sort(c(x, x.q2))
	  }
	  
	  fx <- dgamma(xu - x, fit$mirrorgamma[ex,1],
	               fit$mirrorgamma[ex,2])  
	  
	  if(fit$mirrorgamma[ex,1] ==1){
	  dist.title = paste("Mirror gamma(",
	                     signif(fit$mirrorgamma[ex,1], sf),
	                     ", ",
	                     signif(fit$mirrorgamma[ex,2], sf),
	                     ") (mirror exponential)", sep="")}else{
	                       dist.title = paste("Mirror gamma(",
	                                          signif(fit$mirrorgamma[ex,1], sf),
	                                          ", ",
	                                          signif(fit$mirrorgamma[ex,2], sf),
	                                          ")", sep="") 
	                     }
	} 
	
  if(d == "mirrorlognormal"){
    xu <- fit$limits[ex, 2]
    if(pl == -Inf){pl <- xu - qlnorm(0.999, fit$mirrorlognormal[ex,1],
                                     fit$mirrorlognormal[ex,2])}
    if(pu == Inf){pu <- xu}
    x <- seq(from = pl, to = pu, length = 200)
    if(is.na(ql) == F){
      x.q1 <- xu - qlnorm(1 - ql,
                          fit$mirrorlognormal[ex,1],
                          fit$mirrorlognormal[ex,2])
      x <- sort(c(x, x.q1))}
    if(is.na(qu) == F){
      x.q2 <- xu - qlnorm(1 - qu,
                          fit$mirrorlognormal[ex,1],
                          fit$mirrorlognormal[ex,2])
      x <- sort(c(x, x.q2))}
    
    fx <- dlnorm(xu - x, fit$mirrorlognormal[ex,1],
                 fit$mirrorlognormal[ex,2])
    
    dist.title = paste("Mirror log normal(",
                       signif(fit$mirrorlognormal[ex,1], sf),
                       ", ",
                       signif(fit$mirrorlognormal[ex,2], sf), ")",
                       sep="")
  }
  
  if(d == "mirrorlogt"){ # mirror log student t
    xu <- fit$limits[ex, 2]
   
    # Calculate axes limits using the  mirror lognormal; log-t limits may be too extreme
    if(pl == -Inf){pl <- xu - qlnorm(0.999, 
                                     fit$mirrorlognormal[ex,1],
                                     fit$mirrorlognormal[ex,2])}
    if(pu == Inf){pu <- xu}
    
    x <- seq(from = pl, to = 0.99*xu, length = 200)
    if(is.na(ql) == F){
      x.q1 <- xu - exp(fit$mirrorlogt[ex,1] + 
                         fit$mirrorlogt[ex,2] * qt(1 - ql, fit$mirrorlogt[ex,3]))
      x <- sort(c(x, x.q1))}
    
    if(is.na(qu) == F){
      x.q2 <- 
        xu - exp(fit$mirrorlogt[ex,1] + 
                   fit$mirrorlogt[ex,2] * qt(1 - qu, fit$mirrorlogt[ex,3]))
      
      x <- sort(c(x, x.q2))}
    
    fx <- dt( (log(xu - x) - fit$mirrorlogt[ex,1]) /
                fit$mirrorlogt[ex,2], 
              fit$mirrorlogt[ex,3]) / ((xu - x) *
                                         fit$mirrorlogt[ex,2])
    dist.title = paste("Mirror log T(",
                       signif(fit$mirrorlogt[ex,1], sf),
                       ", ",
                       signif(fit$mirrorlogt[ex,2], sf),
                       "), df = ",
                       fit$mirrorlogt[ex,3], sep="")
    
  }	
  
  
   
	df1 <- data.frame(x = x, fx = fx)
	p1 <- ggplot(df1, aes(x = x, y = fx)) +
	  geom_line(size = lwd) +
	  labs(title = dist.title, x = xlab, y = ylab )+
	  theme(plot.title = element_text(hjust = 0.5))
	if(is.na(ql) == F  ){
	  p1 <- p1 + geom_ribbon(data = subset(df1, x<=x.q1), 
	                         aes(ymax = fx, ymin = 0),
	                         fill = "red",
	                         alpha = 0.5)
	}
	if(is.na(qu) == F ){
	  p1 <- p1 + geom_ribbon(data = subset(df1, x>=x.q2), 
	                         aes(ymax = fx, ymin = 0),
	                         fill = "red",
	                         alpha = 0.5)
	}
	
	if(percentages){
	  p1 <- p1 + scale_x_continuous(labels = scales::percent,  
	                                limits = c(pl, pu))
	}else{
	  p1 <- p1 + xlim(pl, pu)
	}
	
	p1
}
