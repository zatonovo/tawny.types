\name{EquityIndex}
\alias{EquityIndex}
\title{ Create an EquityIndex object }
\description{
A wrapper around equity indices.
}
\usage{
EquityIndex(...)
}
\arguments{
  \item{\dots}{Used by lambda.r}
}
\section{Additional Usage}{
This type takes an index and will download all constituent returns (assuming
the index composition exists). This uses quantmod as a back-end.

EquityIndex(ticker = "^GSPC", hint = NA, src = "yahoo")

ticker - The ticker representing the index\cr
hint - Used to determine number of elements in index for paging\cr
src - Placeholder for data source. Currently only supports yahoo\cr
}
\value{
An EquityIndex object

TODO: Consolidate with BenchmarkPortfolio
}
\author{
Brian Lee Yung Rowe
}

\examples{
  \dontrun{index <- EquityIndex()}
}
