gpt<-function(y,s,alpha1,alpha2,q){
  za1<-qnorm(alpha1/2,0,1,lower.tail=F)
  za1
  za2<-qnorm(alpha2/2,0,1,lower.tail=F)
  za2

  I1<-(abs(y/s)>za1)
  I1

  I2<-((abs(y/s)>za2)&(abs(y/s)<=za1))
  I2

  #pretest(PT) estimator
  PT<-y*I1
  PT

  #general pretest(GPT) estimator
  GPT<-y*I1+y*I2*q
  GPT

  GPT_table<-data.frame("PT"=PT,"GPT"=GPT)
  GPT_table

}
