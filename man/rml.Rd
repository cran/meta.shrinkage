\name{rml}
\alias{rml}
\title{
The Ordered Restricted Maximum Likelihood Estimator under Ordered Means
}
\description{
This function provides the restricted maximum likelihood(RML) estimator under ordered means using Pooled-Adjacent-Violators Algorithm(PAVA).
The technical details and examples for this estimator are described in Section 3.2 of Taketomi et al.(2021).
An example shows the application to the COVID-19 data from Pranata et al.(2020).
This appliction is also described in section 5.2 of Taketomi et al.(2021).

}
\usage{
rml(id,y,x)
}

\arguments{
  \item{id}{a vector for titles or indexes of each study. The elements of this vector is numeric or character.}
  \item{y}{a vector of unrestricted estimators}
  \item{x}{a numeric vector for a covariate that is interested in the relationship between ordered means.}

}

\value{
  \item{id}{a vector for titles or indexes of each study.}
  \item{RML}{the oredered restricted estimator for y using PAVA}
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


#proportion of males for each study
x<-c(56.4,63.0,52.0,49.0,62.1,49.5,82.0,58.0,47.9,45.0,62.0)

#Note that in rml function, y is sorted by x before calculating RML estimator.

#indexes of each study
id<-1:length(y)


rml(id,y,x)



}
