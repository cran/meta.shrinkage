\name{rjs}
\alias{rjs}
\title{
Restricted James-Stein(JS) Estimator Under Ordered Means
}
\description{
This function is used to calculate the shrinkage estimator of James-Stein(JS) type under ordered means.
The calculation of this estimator includes pooled-adjacent-violators algorithm(PAVA).
Technical details is described in Taketomi et al.(2021).
An example shows the application to the COVID-19 data from Pranata et al.(2020).
This appliction is also described in section 5.2 of Taketomi et al.(2021).
}
\usage{
rjs(id,y,s,x)
}
\arguments{
  \item{id}{a vector for titles or indexes of each study. The elements of this vector is numeric or character.}
  \item{y}{a vector for estimates of each study}
  \item{s}{a vector for standard errors of y}
  \item{x}{a numeric vector for a covariate that is interested in the relationship between ordered means}
}
\value{
  \item{id}{a vector for titles or indexes of each study}
  \item{RJS}{Restricted JS estimator}
  \item{RJS-plus}{positive-part restricted JS estimator}
}
\references{
Taketomi N, Konno Y, Chang YT , Emura T (2021). A Meta-Analysis for Simultaneously Estimating Individual Means with Shrinkage, Isotonic Regression and Pretests. Axioms. 10. 267. 10.3390/axioms10040267.

Pranata, R., Lim, M.A., Huang, I., Raharjo, S.B., Lukito, A.A. (2020). Hypertension is associated with increased mortality and severity of disease in COVID-19 pneumonia: a systematic review, meta-analysis and meta-regression. Journal of the Ren-in-Angiotensin-Aldosterone System 21(2).
}
\author{
Nanami Taketomi, Takeshi Emura
}
\examples{
###Application to the COVID-19 data###

#Unrestricted estimates in 11 studys
y<-c(0.6881,0.5933,1.1756,0.5365,0.678,0.5878,0.4637,0.5247,1.2326,2.8904,1.1378)

#standard error of estimates
s<-c(0.6732,0.2754,0.2821,0.2493,0.1713,0.3302,0.0956,0.3272,0.1489,1.4263,0.2097)

#the proportion of Males of each study
x<-c(56.4,63.0,52.0,49.0,62.1,49.5,82.0,58.0,47.9,45.0,62.0)


#Note that in rjs function, y is sorted by x before calculating RML estimator.

#indexes of each study
id<-c("Akbari 2020", "Bai 2000", "Cao 2020", "Chen 2020", "Chen T 2020", "Fu 2020",
         "Grasselli 2020", "Li 2020", "Luo 2020", "Yuan 2020", "Zhou 2020"
)

rjs(id,y,s,x)


}

