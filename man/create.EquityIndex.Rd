\name{create.EquityIndex}
\alias{create.EquityIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.EquityIndex(T, ticker = "^GSPC", hint = NA, src = "yahoo")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{ticker}{
%%     ~~Describe \code{ticker} here~~
}
  \item{hint}{
%%     ~~Describe \code{hint} here~~
}
  \item{src}{
%%     ~~Describe \code{src} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (T, ticker = "^GSPC", hint = NA, src = "yahoo") 
{
    if (is.na(hint)) {
        hints <- c(500, 30, 102, 42)
        names(hints) <- c("^GSPC", "^DJI", "^FTSE", "^HSI")
        hint <- hints[ticker]
    }
    base <- "http://download.finance.yahoo.com/d/quotes.csv?s=@"
    formats <- "&f=sl1d1t1c1ohgv&e=.csv&h="
    comp <- NULL
    pages = max(1, hint\%/\%50)
    for (page in 1:pages) {
        start <- (page - 1) * 50 + 1
        url <- paste(base, ticker, formats, start, sep = "")
        logger(INFO, sprintf("Loading page \%s for \%s", page, 
            ticker))
        data <- read.csv(url, header = FALSE)
        idx = 2
        if (page == 1) {
            idx = 1
        }
        comp <- rbind(comp, data[idx:anylength(data), ])
    }
    as.character(comp[, 1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
