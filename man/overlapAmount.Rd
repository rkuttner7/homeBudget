% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quarterlyIncome.R
\name{quarterly_income}
\alias{quarterly_income}
\title{quarterly_income}
\usage{
quarterly_income(dt, intervals, units = "days")
}
\arguments{
\item{dt}{data.table of income data. Must include columns
`Amount` the earning for that interval, `DateStart` the start
date for earning that salary, `DateEnd` the end date for earning
that salary.}

\item{intervals}{lubridate interval object with specified start and end dates.}

\item{units}{character string. Units in which the results are desired.}
}
\description{
Find the dollars earned over the time period defined in the
 interval for each salary position in the data.table.
}
\examples{
  qrt_int <- quarterly_intervals(end = as.Date("2019-12-31"),
                                 cutoff = as.Date("2019-09-01"))
  quarterly_income(homeBudget::incomeExample, qrt_int)

}
\seealso{
\code{\link[lubridate:interval]{name}}
}
