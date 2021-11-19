\name{gpt}
\alias{gpt}
\title{
The General Pretest(GPT) Estimator for Sparse Means
}
\description{
This function is used to calculate the general pretest(GPT) estimator for individual means under sparse means.
Methodology is described in detail in section 3.3 of Taketomi et al. (2021).
Shih JH et al.(2021) developed mathematical details of the GPT estimator.
An example shows application of this method to the gastric cancer data .
}
\usage{
gpt(y,s,alpha1,alpha2,q)
}
\arguments{
  \item{y}{a vector for estimates}
  \item{s}{a vector for standard error of y}
  \item{alpha1}{significance level for testing (0<alpha1<1)}
  \item{alpha2}{significance level for testing(0<alpha2<1)}
  \item{q}{degrees of shrinkage(0<q<1)}
}
\value{
  \item{PT}{pretest(PT) estimator for y}
  \item{GPT}{general pretest(GPT) estimator for y}
}
\references{
Taketomi N, Konno Y, Chang YT , Emura T (2021). A Meta-Analysis for Simultaneously Estimating Individual Means with Shrinkage, Isotonic Regression and Pretests. Axioms. 10. 267. 10.3390/axioms10040267.

Shih JH, Konno Y, Chang YT, Emura T (2021). A class of general pretest estimators for the univariate normal mean, Communications in Statistics - Theory and Methods, doi: 10.1080/03610926.2021.1955384.

}
\author{Nanami Taketomi, Takeshi Emura}
\examples{
#estimates
y<-c(-0.18312,-0.72266,-0.48507,-0.23961,-0.13226,-0.27228,-0.5867,-0.13969,
-0.1004,-0.31143,-0.04949,-0.11685,-0.13044,0.04391)

#standard error
s<-c(0.15372,0.28686,0.33192,0.21558,0.14691,0.14416,0.24885,
0.14542,0.16404,0.17038,0.19818,0.16476,0.19268,0.17632)

#Pretest(PT) estimator and general pretest(GPT) estimator
gpt(y,s,0.05,0.10,1/2)


}
