roulette<-function(lower=0, upper=100, gridheight=10, nbins=10, round.end = F){
  
  chips<-rep(0,nbins)
  bin.width<-(upper-lower)/nbins
  bin.left<-seq(from=lower,to=upper-bin.width,length=nbins)
  bin.right<-seq(from=lower+bin.width,to=upper,length=nbins)
  xy<-list(x=lower,y=0)

  while(xy$x>= lower & xy$x<= upper){

      par(mfrow=c(1,1))
      plot(c(lower,upper),c(0,0),xlim=c(lower,upper),ylim=c(-1,max(gridheight,max(chips)+1)),type="l",ylab="",xaxp=c(lower,upper,nbins), main = paste("Total chips:", sum(chips)), xlab="Click outside the x-axis range to finish")
      for(i in 1:nbins){
        lines(c(bin.left[i],bin.left[i]),c(0,max(gridheight,max(chips)+1)),lty=3,col=8)
      }
      lines(c(bin.right[nbins],bin.right[nbins]),c(0,max(gridheight,max(chips)+1)),lty=3,col=8)

    for(i in 1:gridheight){
      lines(c(lower,upper),c(i,i), lty=3,col=8)
    }
      
    for(i in 1:nbins){
        if(chips[i]>0){
          rect(rep(bin.left[i],chips[i]),c(0:(chips[i]-1)),rep(bin.right[i],chips[i]),c(1:chips[i]),col=2)
        }
      }

    xy<-locator(n=1)
    if(length(xy)==0){xy=list(x=-1,y=-1)}
  
    if(xy$x > lower & xy$x <upper & xy$y < gridheight){
      index<-ceiling(xy$x/upper*nbins)
      chips[index]<-ceiling(max(xy$y,0))}
    }

  outputs<-list(v=bin.right, p=cumsum(chips)/sum(chips))
  if(round.end == T){
    index <- outputs$p>0 & outputs$p<1
    outputs <- list(outputs$v[index], outputs$p[index])
  }
  
  # Redraw final plot without x-axis label
  
  plot(c(lower,upper),c(0,0),xlim=c(lower,upper),ylim=c(-1,max(gridheight,max(chips)+1)),type="l",ylab="",xaxp=c(lower,upper,nbins), main = paste("Total chips:", sum(chips)), xlab="")
  for(i in 1:nbins){
    lines(c(bin.left[i],bin.left[i]),c(0,max(gridheight,max(chips)+1)),lty=3,col=8)
  }
  lines(c(bin.right[nbins],bin.right[nbins]),c(0,max(gridheight,max(chips)+1)),lty=3,col=8)
  
  for(i in 1:gridheight){
    lines(c(lower,upper),c(i,i), lty=3,col=8)
  }
  
  for(i in 1:nbins){
    if(chips[i]>0){
      rect(rep(bin.left[i],chips[i]),c(0:(chips[i]-1)),rep(bin.right[i],chips[i]),c(1:chips[i]),col=2)
    }
  }
  
  outputs
}    