% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gainsplot.R
\name{gainsplot}
\alias{gainsplot}
\title{A function to plot a gains curve}
\usage{
gainsplot(label.var, ...)
}
\arguments{
\item{label.var}{(required)}

\item{score1, }{score 2, ... (at least 1 required)}
}
\description{
This function allows you to compare model performace by comparing the gains curves of models.
}
\examples{
gainsplot(logit1$fitted.values, rf$fitted.values, nn$fitted.values, label.var = bbb$buyer)
}
\keyword{auc}
\keyword{gains,}
