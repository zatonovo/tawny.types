\name{create.AssetReturns}
\alias{create.AssetReturns}
\title{ Create an AssetReturns object }
\description{
AssetReturns contains some meta-data for asset returns objects.
}
\usage{
create.AssetReturns(T, symbols, obs = NULL, start = NULL, end = Sys.Date(), fun = function(x) Delt(Cl(x)), reload = FALSE, na.value = NA, ...)
}
\arguments{
  \item{T}{AssetReturns}
  \item{symbols}{ }
  \item{obs}{ }
  \item{start}{ }
  \item{end}{ }
  \item{fun}{ }
  \item{reload}{ }
  \item{na.value}{ }
  \item{\dots}{ }
}
\details{
This is a thin wrapper around a zoo object containing portfolio returns. It
is used to type the zoo object based on the business domain, as opposed to the
programming mechanics.
}
\value{
An AssetReturns object
}
\author{
Brian Lee Yung Rowe
}

\examples{
  # This is run automatically in the package
  tawny.types:::.init()

  returns <- create(AssetReturns, c('BAC','JPM','C','GS','MS'), 90)
}
