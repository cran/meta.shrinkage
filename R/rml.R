rml<-function(id,y,x){
  G<-length(y)
  study<-1:G
  data<-data.frame("id"=id,"y"=y,"x"=x)
  data
  dat<-data[order(data$x,decreasing = T),]
  dat

  box<-NULL
  for (i in 1:G) {
    r<-length(1:i)
    c<-length(i:G)
    x<-matrix(0, nrow=r, ncol=c)
    min<-NULL
    for (s in 1:i) {
      for (t in i:G) {
        x[s,(t-(i-1))]<-sum(y[s:t])/(t-s+1)
      }
      min[s]<-min(x[s,])
    }
    box<-rbind(box,max(min))
  }

  RML_table<-data.frame("id"=dat$id,"RML"=box)

  return(RML_table)
}
