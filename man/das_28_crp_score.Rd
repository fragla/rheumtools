% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/das.R
\name{das_28_crp_score}
\alias{das_28_crp_score}
\title{Calculate DAS28-CRP scores}
\usage{
das_28_crp_score(tjc, sjc, crp, ptgh, digits = 2, ignore = TRUE)
}
\arguments{
\item{tjc}{numeric Tender 28 joint count (0 to 28).}

\item{sjc}{numeric Swollen 28 joint count (0 to 28).}

\item{crp}{numeric C-reactive protein (CRP) in mg/l.}

\item{ptgh}{numeric Patient Global Health VAS score (0 to 100mm).}

\item{digits}{numeric specifying the number of decimal places. Defaults to 2.}

\item{ignore}{boolean ignore incorrect parameter values and return NA.}
}
\value{
DAS28-CRP score.
}
\description{
Calculate the Disease Activity Score 28 (DAS28-CRP) for Rheumatoid
 Arthritis with CRP.
}
\examples{
das_28_crp_score(tjc = 4, sjc = 5, crp = 7, ptgh = 50)
das_28_crp_score(tjc = 28, sjc = 28, crp = 10, ptgh = 100)

}
