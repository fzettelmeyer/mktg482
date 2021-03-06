% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uplift.R
\name{QiniBarPlot}
\alias{QiniBarPlot}
\title{A function to plot a Qini Bar Plot for two models.}
\usage{
QiniBarPlot(..., modelnames = NULL)
}
\arguments{
\item{modelnames}{= c("model1", "model2") (optional)}

\item{table1, }{table2, (exactly)}
}
\description{
This function allows you to compare model performace by comparing the Qini Bar Plot of two models.
}
\examples{
QiniBarPlot(PerfTable_uplift, PerfTable_propensity, modelnames = c("Logit Uplift", "Logit Propensity"))
}
