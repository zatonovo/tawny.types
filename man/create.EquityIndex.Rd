\name{create.EquityIndex}
\alias{create.EquityIndex}
\title{ Create an EquityIndex object }
\description{
}
\usage{
create.EquityIndex(T, ticker = "^GSPC", hint = NA, src = "yahoo")
}
\arguments{
  \item{T}{EquityIndex}
  \item{ticker}{ The ticker representing the index }
  \item{hint}{ }
  \item{src}{ }
}
\details{
}
\value{
An EquityIndex object
}
\author{
Brian Lee Yung Rowe
}

\examples{
  # This is run automatically in the package
  tawny.types:::.init()

  index <- create(EquityIndex)
}
