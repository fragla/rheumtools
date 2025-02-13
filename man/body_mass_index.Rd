% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi.R
\name{body_mass_index}
\alias{body_mass_index}
\title{Calculate Body Mass Index}
\usage{
body_mass_index(weight, height, metric = TRUE, digits = 1, ignore = TRUE)
}
\arguments{
\item{weight}{numeric weight.}

\item{height}{numeric height.}

\item{metric}{logical metric units (kilograms/metres) rather than imperial
(pounds/inches) Default is TRUE.}

\item{digits}{numeric the number of decimal places. Defaults to 1.}

\item{ignore}{boolean ignore incorrect parameter values and return NA.}
}
\value{
Body Mass Index
}
\description{
Calculate Body Mass Index using weight in kilograms and height in metres.
}
\examples{
body_mass_index(weight = 70, height = 1.75)

}
