% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read and print fars_data for a particular year.}
\usage{
fars_read(filename)
}
\arguments{
\item{Input}{class 'data.frame' in .csv format, .csv within ZIP format as argument.}
}
\value{
This function returns the input data file in Tibble format
along with the variable names in the data file.
}
\description{
This is a function, that reads the input data frame. Firstly
it checks whether the data exists in the specified directory or NOT. If the data
exists, then it is printed out in Tibble format along with the variable names.
}
\examples{
\dontrun{
fars_read("accident_2013.csv.bz2")
fars_read("accident_2015.csv")
}

}
