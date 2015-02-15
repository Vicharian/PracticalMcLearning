xp=function(x){
  #par(ask=T)
  for (i in 1:ncol(x)) {
    png(paste0("Plot",i,".png"), height=480, width=480)
    print(qplot(data=x,x[,i],xlab=names(x)[i]))
    dev.off()
  }
  #par(ask=F)
}