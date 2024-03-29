% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_formatting.R
\name{allYears}
\alias{allYears}
\title{Subset data so only nodes with observations in every year are included}
\usage{
allYears(df)
}
\arguments{
\item{df}{A dataframe with three columns denoting nodes, years, and values, respectively}
}
\value{
A dataframe with three columns (node, year, and value)
}
\description{
Subset data so only nodes with observations in every year are included
}
\examples{
allYears(polityData)

}
