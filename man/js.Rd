\name{js}
\alias{js}
\title{James-Stein (JS) Estimator and Positive-Part JS Estimator for Means}
\description{
This function computes the James-Stein (JS) shirinkage estimators for means.
The detail of this estimation is described in Section 3.1 of Taketomi et al. (2021).
An example shows the application of this method to the gastric cancer data of GASTRIC group (2013).
}

\usage{
js(y,s)
}
\arguments{
  \item{y}{a vector for estimates}
  \item{s}{a vector for standard errors of y}
}
\value{
  \item{JS}{James-Stein(JS) estimator for y}
  \item{JS_plus}{positive-part JS estimator for y}
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

#JS estimator and JS-plus estimator
js(y,s)

}

\keyword{Shrinkage estimator}
\keyword{James-Stein (JS) estimator}
