cor2bed<-function(cor){
  cor<-as.character(cor)
  a<-unlist(lapply(strsplit(cor,split=c(":")),function(x) strsplit(x,"-")))
  bed<-matrix(a,ncol=3,byrow=T)
  return(data.frame(bed))
}
