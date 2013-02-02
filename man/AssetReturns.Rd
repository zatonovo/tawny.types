\name{AssetReturns}
\alias{AssetReturns}
\title{ Create an AssetReturns object }
\description{
AssetReturns contains some meta-data for asset returns objects.
}
\usage{
AssetReturns(...)
}
\arguments{
  \item{\dots}{Used by lambda.r}
}
\details{
This is a thin wrapper around a zoo object containing portfolio returns. It
is used to type the zoo object based on the business domain, as opposed to the
programming mechanics.

AssetReturns(symbols, obs = NULL, start = NULL, end = Sys.Date(), 
  fun = function(x) Delt(Cl(x)), reload = FALSE, na.value = NA, ...)

symbols - A vector of symbols to load

obs - Number of total observations to load

start - Start date of data to load

end - End date of data to load

fun - Function to apply to calculate returns

reload - Should existing downloaded data be reloaded?

na.value - Value to use for NAs

\dots - Additional arguments
}
\value{
An AssetReturns object
}
\author{
Brian Lee Yung Rowe
}

\examples{
  \dontrun{returns <- AssetReturns(c('BAC','JPM','C','GS','MS'), 90)}
}
