% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hfrs_group.R
\name{hfrs_group}
\alias{hfrs_group}
\title{HFRS_group function
This function assigns the risk category (Low, Intermediate, High) to Hospital Frailty Risk Score data}
\usage{
hfrs_group(df, hfrs)
}
\arguments{
\item{df}{The data frame containing ICD codes}

\item{hfrs}{the variable for HFRS score}
}
\description{
HFRS_group function
This function assigns the risk category (Low, Intermediate, High) to Hospital Frailty Risk Score data
}
\examples{
data$HFRS_category<-hfrs_group(dataframe, "hfrs_score")
hfrs_group()
}
\keyword{frailty,}
\keyword{hfrs}
\keyword{risk,}
