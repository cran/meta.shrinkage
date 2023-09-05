\name{rml}
\alias{rml}
\title{
The Ordered Restricted Maximum Likelihood Estimator under Ordered Means
}
\description{
This function provides the restricted maximum likelihood (RML) estimator under ordered means using Pooled-Adjacent-Violators Algorithm (PAVA).
The technical details and examples for this estimator are described in Section 3.2 of Taketomi et al. (2021).
An example shows the application to the COVID-19 data from Pranata et al. (2020).
This appliction is also described in Section 5.2 of Taketomi et al. (2021).

}
\usage{
rml(y,x=1:length(y),id=1:length(y),decreasing=FALSE)
}

\arguments{
  \item{y}{a vector of estimates}
  \item{x}{a numeric vector for a covariate that is interested in the relationship between ordered means. Default implies the serial number assigned to the dataset.}
  \item{id}{a vector for the names of studies. The elements of this vector is numeric or character.}
  \item{decreasing}{logical scalar - Whether to sort the dataset in decreasing order by x or not. If decreasing=TRUE, RML in the output is estimators under the assumption that the y is monotonically decreasing with respect to x.}
}

\value{
  \item{id}{the names of studies}
  \item{x}{a numeric vector for a covariate that is used to sort the dataset.}
  \item{RML}{the ordered restricted estimator for y using PAVA}
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

#Proportions of males of each study
x<-c(56.4,63.0,52.0,49.0,62.1,49.5,82.0,58.0,47.9,45.0,62.0)


#Under the assumption that y is monotonically decreasing with respect to x,
#input decreasing=TRUE(the dataset is sorted by in decreasing order by x).
rml(y,x,decreasing=TRUE)


#If x and decreasing are default, the dataset is sorted by
#in the serial numbers of studies
#and RML in the output is under the assumption
#that y is monotonically increasing with respect to serial numbers.
rml(y)



#If x is default and decreasing=TRUE, the dataset is sorted by
#in decreasing the serial numbers of studies
#and RML in the output is under the assumption
#that y is monotonically decreasing with respect to serial numbers.
rml(y,decreasing=TRUE)


}


\keyword{Ordered means}
\keyword{Isotonic regression}
\keyword{Restricted maximum likelihood (RML) estimator}
\keyword{Pooled-Adjacent-Violators Algorithm (PAVA)}
