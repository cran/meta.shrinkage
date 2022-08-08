gpt<-function(y,s,alpha1=0.05,alpha2=0.10,level=0.05,q=0.5,conf.int=FALSE,conf.type="pivot"){
  za1<-qnorm(alpha1/2,0,1,lower.tail=FALSE)
  za2<-qnorm(alpha2/2,0,1,lower.tail=FALSE)
  zl<-qnorm(level/2,0,1,lower.tail = FALSE)

  G=length(y)

  I1<-(abs(y/s)>za1)
  I2<-((abs(y/s)>za2)&(abs(y/s)<=za1))

  #pretest(PT) estimator
  PT<-y*I1

  #general pretest(GPT) estimator
  GPT<-y*I1+y*I2*q


  if(conf.int==TRUE){
    if(conf.type=="pivot"){
      lower.pt.pivot<-rep(NA,G)
      upper.pt.pivot<-rep(NA,G)
      lower.pt.wald<-rep(NA,G)
      upper.pt.wald<-rep(NA,G)

      lower.pt.pivot<- (-za1-zl)*s*(PT==0)+(PT-zl*s)*(PT<(-za1*s))+(PT-zl*s)*(za1*s<PT)
      upper.pt.pivot<- (za1+zl)*s*(PT==0)+(PT+zl*s)*(PT<(-za1*s))+(PT+zl*s)*(za1*s<PT)

      res<-data.frame(PT,GPT,lower.pt.pivot, upper.pt.pivot)
      res

    }

    else if(conf.type=="wald"){
      lower.pt.wald<-PT-zl*s
      upper.pt.wald<-PT+zl*s

      res<-data.frame(PT,GPT,lower.pt.wald, upper.pt.wald)
      res
    }
  }

  else if(conf.int==FALSE){
    res<-data.frame(PT,GPT)
    res
    }

}
