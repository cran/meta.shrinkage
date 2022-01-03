
js <- function(y,s) {
  #the number of studies
  G<-length(y)


  #JS estimator
  p<-(1-(G-2)/(sum(y^2/s^2)))
  JS<-p*y
  JS


  #JS+ estimator
  q<-ifelse(p>0,p,0)
  JS_plus<-q*y
  JS_plus

  #table
  JS_table<-data.frame("JS"=JS,"JS_plus"=JS_plus)
  return(JS_table)

}
