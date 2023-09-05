\name{gpt}
\alias{gpt}
\title{
The General Pretest (GPT) Estimator for Sparse Means
}
\description{
This function is used to calculate the general pretest (GPT) estimator for individual means under sparse means.
As an option, confidence intervals corresponding to pretest estimators can be computed.
The methodology is described in detail in Section 3.3 of Taketomi et al. (2021), Section 3.4 of Taketomi et al. (2022), and Section 2 and Section 3 of Taketomi et al. (2023-).
An example shows the application of this method to the gastric cancer data of GASTRIC group (2013) .
}
\usage{
gpt(y,s,alpha1=0.05,alpha2=0.10,level=0.05,q=0.5,conf.int=FALSE,conf.type="pivot")
}
\arguments{
  \item{y}{a vector for estimates}
  \item{s}{a vector for standard errors of y}
  \item{alpha1}{significance level for pretest (0<alpha1<1)}
  \item{alpha2}{significance level for pretest (0<alpha2<1)}
  \item{level}{a constant such that 1-level is confidence level}
  \item{q}{degrees of shrinkage(0<q<1)}
  \item{conf.int}{an indicator whether confidence intervals for pretest estimators are in the output}
  \item{conf.type}{an indicator that implies which type of confidence intervals for pretest estimators is in the output.
  Default is "pivot".The other type is "wald".}
}
\value{
  \item{PT}{pretest(PT) estimator for y}
  \item{GPT}{general pretest(GPT) estimator for y}
  \item{lower.pt.pivot}{Lower limits for pivoting type.}
  \item{upper.pt.pivot}{Upper limits for pivoting type.}
  \item{lower.pt.wald}{Lower limits for Wald type.}
  \item{upper.pt.wald}{Upper limits for Wald type.}

}
\references{
Taketomi N, Konno Y, Chang YT and Emura T (2021). A meta-analysis for simultaneously estimating individual means with shrinkage, isotonic regression and pretests. Axioms. 10. 267. 10.3390/axioms10040267.

Taketomi N, Michimae H, Chang YT and Emura T (2022). meta.shrinkage: An R Package for Meta-Analyses for Simultaneously Estimating Individual Means. Algorithms. 15. 26.

Taketomi N, Chang YT, Konno Y, Mori M and Emura T (2023-). Confidence interval for normal means in meta-analysis based on a pretest estimator. Under review.

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

#If conf.int=TRUE, confidence intervals fot PT are in the output.
#Default is 95\% confidence interval in pivot type.
gpt(y,s,conf.int=TRUE)

}

\keyword{Pretest estimator}
\keyword{Sparse means}
\keyword{shrinkage estimator}
\keyword{Confidence intervals for individual means}
