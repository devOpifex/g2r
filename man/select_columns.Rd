% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{select_columns}
\alias{select_columns}
\title{Select Columns}
\usage{
select_columns(data = NULL, cols)
}
\arguments{
\item{data}{A data.frame.}

\item{cols}{Column names to select.}
}
\description{
Select specific columns from a data.frame, if
they exist.
}
\examples{
\dontrun{
select_columns(cars, c("notExist", "speed"))
}

}
\keyword{internal}
