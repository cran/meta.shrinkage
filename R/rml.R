rml<-function(y,x=1:length(y),id=1:length(y),decreasing=FALSE){
  G<-length(y)
  data<-data.frame("id"=id,"y"=y,"x"=x)
  data
  dat<-data[order(data$x,decreasing = decreasing),]
  dat

  box<-NULL
  for (i in 1:G) {
    z<-matrix(0, nrow=i, ncol=(G-(i-1)))
    min<-NULL
    for (s in 1:i) {
      for (u in i:G) {
        z[s,(u-(i-1))]<-sum(dat$y[s:u])/(u-s+1)
      }
      min[s]<-min(z[s,])
    }
    box<-rbind(box,max(min))
  }

  RML_table<-data.frame("id"=dat$id,"x"=dat$x,"RML"=box)

  return(RML_table)
}
