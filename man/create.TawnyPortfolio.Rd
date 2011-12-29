\name{create.TawnyPortfolio}
\alias{create.TawnyPortfolio}
\alias{start.TawnyPortfolio}
\alias{end.TawnyPortfolio}
\alias{rollapply.TawnyPortfolio}
\title{ Create a TawnyPortfolio object }
\description{
  Represents a portfolio. Contains information about the portfolio composition,
  returns, window, etc.
}
\usage{
create.TawnyPortfolio(...)
}
\arguments{
  \item{\dots}{ Arguments to the constructor. See below for details }
}
\details{
%create.TawnyPortfolio.ret(T, returns, window = 90)
%create.TawnyPortfolio.sym(T, symbols, window = 90, obs = 150)
}
\value{
}
\author{
Brian Lee Yung Rowe
}

\examples{
  # This is run automatically in the package
  tawny.types:::.init()

  port <- create(TawnyPortfolio, c('BAC','JPM','C','GS','MS'))
}
