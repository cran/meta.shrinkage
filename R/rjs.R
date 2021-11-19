#---------------
# RJS
#---------------

rjs<-function(id,y,s,x){

data<-data.frame("id"=id,"y"=y,"s"=s,"x"=x)
data
dat<-data[order(data$x,decreasing = T),]

  G<-length(dat$y)
  study<-1:G
  RML_est<-NULL
  for (i in 1:G) {
    r<-length(1:i)
    c<-length(i:G)
    x<-matrix(0, nrow=r, ncol=c)
    min<-NULL
    for (j in 1:i) {
      for (t in i:G) {
        x[j,(t-(i-1))]<-sum(y[j:t])/(t-j+1)
      }
      min[j]<-min(x[j,])
    }
    RML_est<-rbind(RML_est,max(min))
  }



iy<-matrix(0, nrow=G, ncol=G)
for (i in 1:G) {
  for (j in 1:G) {
    if((dat$y[i]<=dat$y[j])&&(i<j)){
      iy[i,j]<-T
    }
    else {iy[i,j]<-F}
  }
}

iy1<-sum(iy)==sum(1:(G-1))

iy2<-1-(sum(iy)==sum(1:(G-1)))

RJS<-(1-(G-2)/(sum((dat$y)^2/s^2)))*(dat$y)*iy1+RML_est*iy2

RJS_plus<-((1-(G-2)/(sum((dat$y)^2/s^2)))>0)*(dat$y)*iy1+RML_est*iy2


rjs_table<-data.frame("id"=dat$id,"RJS"=RJS,"RJS-plus"=RJS_plus)

return(rjs_table)

}
