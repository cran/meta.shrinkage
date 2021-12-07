\name{gpt}
\alias{gpt}
\title{
The General Pretest(GPT) Estimator for Sparse Means
}
\description{
This function is used to calculate the general pretest(GPT) estimator for individual means under sparse means.
The methodology is described in detail in Section 3.3 of Taketomi et al. (2021).
An example shows the application of this method to the gastric cancer data of GASTRIC group (2013) .
}
\usage{
gpt(y,s,alpha1=0.05,alpha2=0.10,q=0.5)
}
\arguments{
  \item{y}{a vector for estimates}
  \item{s}{a vector for standard errors of y}
  \item{alpha1}{significance level for testing (0<alpha1<1)}
  \item{alpha2}{significance level for testing(0<alpha2<1)}
  \item{q}{degrees of shrinkage(0<q<1)}
}
\value{
  \item{PT}{pretest(PT) estimator for y}
  \item{GPT}{general pretest(GPT) estimator for y}
}
\references{
Taketomi N, Konno Y, Chang YT , Emura T (2021). A meta-analysis for simultaneously estimating individual means with shrinkage, isotonic regression and pretests. Axioms. 10. 267. 10.3390/axioms10040267.

GASTRIC group (2013). Role of chemotherapy for advanced/recurrent gastric cancer: An individual-patient-data meta-analysis, European Journal of Cancer 49 (7): 1565-1577. doi:10.1016/j.ejca.2012.12.016.

}
\author{Nanami Taketomi, Takeshi Emura}
\examples{
#Estimates from the gastric cancer studies(Taketomi et al.(2021); GASTRIC group (2013))
y<-c(-0.18312,-0.72266,-0.48507,-0.23961,-0.13226,-0.27228,-0.5867,-0.13969,
-0.1004,-0.31143,-0.04949,-0.11685,-0.13044,0.04391)

#Standard errors(Taketomi et al.(2021))
s<-c(0.15372,0.28686,0.33192,0.21558,0.14691,0.14416,0.24885,
0.14542,0.16404,0.17038,0.19818,0.16476,0.19268,0.17632)

#Pretest(PT) estimator and general pretest(GPT) estimator
gpt(y,s)


}
