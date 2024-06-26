% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IDmother.R
\name{IDmother}
\alias{IDmother}
\title{ID of mother of ego}
\usage{
IDmother(idego, d = NULL)
}
\arguments{
\item{idego}{ID}

\item{d}{Name of database. If d is missing, the dataset dLH in the global environment (R workspace) is used. 
If no dLH in the global environment, the database dLH distributed with the Families package is used.}
}
\value{
ID of mother. Returns NA if ID of mother is not included in the database
}
\description{
Retrieves the ID of mother of ego or mothers of vector of egos
}
\examples{

# load the data
data(dLH,package = "Families")
IDmother (sample(dLH$ID[dLH$gen==2],1),d=dLH)


}
