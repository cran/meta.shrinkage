
js <- function(y,s) {
  #the number of studies
  G<-length(y)


  #JS estimator
  JS<-{1-(G-2)/(sum(y^2/s^2))}*y
  JS


  #JS+ estimator
  p<-((1-(G-2)/(sum(y^2/s^2)))>0)
  w<-1-(G-2)/(sum(y^2/s^2))
  JS_plus<-w*p*y
  JS_plus

  #table
  JS_table<-data.frame("JS"=JS,"JS_plus"=JS_plus)
  return(JS_table)

}
