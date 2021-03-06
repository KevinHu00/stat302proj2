% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_rf_cv.R
\name{my_rf_cv}
\alias{my_rf_cv}
\title{Random Forest Cross-Validation}
\usage{
my_rf_cv(k)
}
\arguments{
\item{k}{Number of folds.}
}
\value{
Numeric with the cross-validation error.
}
\description{
This function predicts output body_mass_g.
}
\examples{
data(my_penguins)
my_rf_cv(3)
my_rf_cv(4)

}
\keyword{prediction}
