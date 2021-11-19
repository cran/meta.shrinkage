\name{js}
\alias{js}
\title{James-Stein(JS) Estimator and Positive-Part JS Estimator for Means}
\description{
This function provides James-Stein(JS)s' shirinkage estimators for means.
The detail of this estimation is described in section 3.1 of Taketomi et al.(2021).
An example shows application of this method to the gastric cancer data.
}

\usage{
js(n,y,s)
}
\arguments{
  \item{n}{a vector for sample sizes of each study}
  \item{y}{a vector for estimates}
  \item{s}{a vector for standard error of y}
}
\value{
  \item{JS}{James-Stein(JS) estimator for y}
  \item{JS_plus}{positive-part JS estimator for y}
}
\references{
Taketomi N, Konno Y, Chang YT , Emura T (2021). A Meta-Analysis for Simultaneously Estimating Individual Means with Shrinkage, Isotonic Regression and Pretests. Axioms. 10. 267. 10.3390/axioms10040267.

}
\author{Nanami Taketomi, Takeshi Emura}
\examples{
#An exmaple for the gastric cancer data

#sample size
n<-c(269,190,252,536,219,306,88,281,271,178,126,180,206,186)

#estimates for each study
y<-c(-0.18312,-0.72266,-0.48507,-0.23961,-0.13226,-0.27228,-0.5867,-0.13969,
-0.1004,-0.31143,-0.04949,-0.11685,-0.13044,0.04391)

#standard error for estimates
s<-c(0.15372,0.28686,0.33192,0.21558,0.14691,0.14416,0.24885,
0.14542,0.16404,0.17038,0.19818,0.16476,0.19268,0.17632)

#JS estimator and JS-plus estimator
js(n,y,s)

}
