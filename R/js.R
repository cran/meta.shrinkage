# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

js <- function(n,y,s) {
  #number of studies
  G<-length(n)


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
