rjs<-function(y,s,x=1:length(y),id=1:length(y),decreasing=FALSE){

data<-data.frame("id"=id,"y"=y,"s"=s,"x"=x)
dat<-data[order(data$x,decreasing = decreasing),]

RML_est<-rml(y,x,decreasing=decreasing)$RML

  G<-length(dat$y)
  study<-1:G

iy<-matrix(0, nrow=G, ncol=G)
for (i in 1:G) {
  for (j in 1:G) {
    if((dat$y[i]<=dat$y[j])&&(i<j)){
      iy[i,j]<-TRUE
    }
    else {iy[i,j]<-FALSE}
  }
}

iy1<-sum(iy)==sum(1:(G-1))

iy2<-1-(sum(iy)==sum(1:(G-1)))

RJS<-(1-(G-2)/(sum((dat$y)^2/s^2)))*(dat$y)*iy1+RML_est*iy2

RJS_plus<-((1-(G-2)/(sum((dat$y)^2/s^2)))>0)*(dat$y)*iy1+RML_est*iy2


rjs_table<-data.frame("id"=dat$id, "x"=dat$x,"RJS"=RJS,"RJS_plus"=RJS_plus)

return(rjs_table)

}
