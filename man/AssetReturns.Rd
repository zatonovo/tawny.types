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
\section{Additional Usage}{
symbols - A vector of symbols to load\cr
obs - Number of total observations to load\cr
start - Start date of data to load\cr
end - End date of data to load\cr
fun - Function to apply to calculate returns\cr
reload - Should existing downloaded data be reloaded?\cr
na.value - Value to use for NAs\cr
\dots - Additional arguments\cr
}
\details{
This is a thin wrapper around a zoo object containing portfolio returns. It
is used to type the zoo object based on the business domain, as opposed to the
programming mechanics.

AssetReturns(symbols, obs = NULL, start = NULL, end = Sys.Date(), 
  fun = function(x) Delt(Cl(x)), reload = FALSE, na.value = NA, ...)

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
