\name{tawny.types-package}
\alias{tawny.types-package}
\alias{tawny.types}
\alias{tawny.options}
\docType{package}
\title{Common types for tawny}
\description{
Base types used throughout tawny
}
\details{
\tabular{ll}{
Package: \tab tawny.types\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2014-01-23\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
Create portfolio objects from these types
}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@zatonovo.com>
}
\keyword{ package }
\seealso{ \code{\link[tawny]{tawny-package}} }
\examples{
\dontrun{
p <- TawnyPortfolio(c('AAPL','GOOG','IBM'), 150,200)
m <- BenchmarkPortfolio('^GSPC', 150,200)
}
}
