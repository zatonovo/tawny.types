\name{tawny.types-package}
\alias{tawny.types-package}
\alias{tawny.types}
\docType{package}
\title{Common types for tawny}
\description{
Base types used throughout tawny
}
\details{
\tabular{ll}{
Package: \tab tawny.types\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-10-22\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
Create portfolio objects from these types
}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@nurometic.com>
}
\keyword{ package }
\seealso{ \code{\link[tawny]{tawny-package}} }
\examples{
# This is run automatically in the package
tawny.types:::.init()

p <- create(TawnyPortfolio, c('AAPL','GOOG','IBM'), 150,200)
m <- create(BenchmarkPortfolio, '^GSPC', 150,200)
}
