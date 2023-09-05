\name{rjs}
\alias{rjs}
\title{
Restricted James-Stein (JS) Estimator Under Ordered Means
}
\description{
This function is used to calculate the James-Stein (JS) shrinkage estimator under ordered means.
The calculation of this estimator includes pooled-adjacent-violators algorithm (PAVA).
Technical details is described in Taketomi et al. (2021).
An example shows the application to the COVID-19 data from Pranata et al. (2020).
This appliction is also described in Section 5.2 of Taketomi et al. (2021).
}
\usage{
rjs(y,s,x=1:length(y),id=1:length(y),decreasing=FALSE)
}
\arguments{
  \item{y}{a vector for estimates }
  \item{s}{a vector for standard errors of y}
  \item{x}{a numeric vector for covariates to define the order of studies. Default implies the serial number assigned to the dataset.}
  \item{id}{a vector for the names of studies. The elements of this vector are numeric or character.}
  \item{decreasing}{logical scalar - Whether to sort the dataset in decreasing order by x or not. }
}
\value{
  \item{id}{the names of studies}
  \item{x}{a numeric vector for a covariate that is used to sort the dataset.}
  \item{RJS}{Restricted JS estimator}
  \item{RJS_plus}{positive-part restricted JS estimator}
}
\references{
Taketomi N, Konno Y, Chang YT , Emura T (2021). A meta-analysis for simultaneously estimating individual means with shrinkage, isotonic regression and pretests. Axioms. 10. 267. 10.3390/axioms10040267.

Pranata R, Lim MA, Huang I, Raharjo SB, Lukito AA (2020). Hypertension is associated with increased mortality and severity of disease in COVID-19 pneumonia: A systematic review, meta-analysis and meta-regression. Journal of the renin-angiotensin-aldosterone system. 21(2). 1470320320926899.
}
\author{
Nanami Taketomi, Takeshi Emura
}
\examples{
#Estimates from the COVID-19 data (Taketomi et al. (2021); Pranata et al. (2020))
y<-c(0.6881,0.5933,1.1756,0.5365,0.678,0.5878,0.4637,0.5247,1.2326,2.8904,1.1378)

#standard errors of estimates
s<-c(0.6732,0.2754,0.2821,0.2493,0.1713,0.3302,0.0956,0.3272,0.1489,1.4263,0.2097)

#The proportions of males of each study
x<-c(56.4,63.0,52.0,49.0,62.1,49.5,82.0,58.0,47.9,45.0,62.0)


#Note that in rjs function, y is sorted by x before calculating RML estimator.

#The names of studies
id<-c("Akbari 2020", "Bai 2000", "Cao 2020", "Chen 2020", "Chen T 2020", "Fu 2020",
         "Grasselli 2020", "Li 2020", "Luo 2020", "Yuan 2020", "Zhou 2020"
)

rjs(y,s,x,id,decreasing=TRUE)


}

\keyword{Ordered means}
\keyword{Isotonic regression}
\keyword{James-Stein (JS) estimator}
\keyword{Pooled-Adjacent-Violators Algorithm (PAVA)}
\keyword{Shrinkage estimator}
