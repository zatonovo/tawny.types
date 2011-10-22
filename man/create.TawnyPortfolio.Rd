\name{create.TawnyPortfolio}
\alias{create.TawnyPortfolio}
\alias{create.TawnyPortfolio.ret}
\alias{create.TawnyPortfolio.sym}
\title{ Create a TawnyPortfolio object }
\description{
}
\usage{
create.TawnyPortfolio(...)
create.TawnyPortfolio.ret(T, returns, window = 90)
create.TawnyPortfolio.sym(T, symbols, window = 90, obs = 150)
}
\arguments{
  \item{T}{ TawnyPortfolio }
  \item{returns}{ }
  \item{symbols}{ }
  \item{window}{ }
  \item{obs}{ }
}
\details{
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
