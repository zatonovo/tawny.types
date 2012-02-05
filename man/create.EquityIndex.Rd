\name{create.EquityIndex}
\alias{create.EquityIndex}
\title{ Create an EquityIndex object }
\description{
A wrapper around equity indices.
}
\usage{
create.EquityIndex(T, ticker = "^GSPC", hint = NA, src = "yahoo")
}
\arguments{
  \item{T}{EquityIndex}
  \item{ticker}{ The ticker representing the index }
  \item{hint}{Used to determine number of elements in index for paging}
  \item{src}{Placeholder for data source. Currently only supports yahoo}
}
\details{
This type takes an index and will download all constituent returns (assuming
the index composition exists). This uses quantmod as a back-end.
}
\value{
An EquityIndex object

TODO: Consolidate with BenchmarkPortfolio
}
\author{
Brian Lee Yung Rowe
}

\examples{
  # This is run automatically in the package
  tawny.types:::.init()

  index <- create(EquityIndex)
}
