% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importance.R
\name{getform.glmnet}
\alias{getform.glmnet}
\title{A function to create a new formula after glmnet in caret}
\usage{
getform.glmnet(modelFit, lambda = modelFit$bestTune$lambda)
}
\arguments{
\item{modelFit}{required: trained caret glmnet model}

\item{lambda}{optional: a lambda penalty (default is modelFit$bestTune$lambda)}
}
\value{
A formula
}
\description{
This function takes non-zero variables after glmnet and returns a
new formula. If any dummy created from a factor is non-zero, it will
incluce the factor variable
}
\examples{
getform.glmnet(glmnetFit)
}
\keyword{LASSO}
\keyword{glmnet}
