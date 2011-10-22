\name{create.AssetReturns}
\alias{create.AssetReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.AssetReturns(T, symbols, obs = NULL, start = NULL, end = Sys.Date(), fun = function(x) Delt(Cl(x)), reload = FALSE, na.value = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{symbols}{
%%     ~~Describe \code{symbols} here~~
}
  \item{obs}{
%%     ~~Describe \code{obs} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{fun}{
%%     ~~Describe \code{fun} here~~
}
  \item{reload}{
%%     ~~Describe \code{reload} here~~
}
  \item{na.value}{
%%     ~~Describe \code{na.value} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (T, symbols, obs = NULL, start = NULL, end = Sys.Date(), 
    fun = function(x) Delt(Cl(x)), reload = FALSE, na.value = NA, 
    ...) 
{
    if (is.null(start) & is.null(obs)) {
        stop("Either obs or start must be set")
    }
    end <- as.Date(end)
    if (is.null(start)) {
        start <- end - (10 + obs * 365/250)
    }
    p <- xts(order.by = end)
    for (s in symbols) {
        asset <- getSymbols(s, from = start, to = end, auto.assign = FALSE)
        raw <- fun(asset)
        logger(INFO, sprintf("Binding \%s for [\%s,\%s]", s, format(start(raw)), 
            format(end(raw))))
        a <- xts(raw, order.by = index(asset))
        p <- cbind(p, a[2:anylength(a)])
    }
    colnames(p) <- symbols
    o.dates <- rownames(p)
    p <- p[apply(p, 1, function(x) sum(x, na.rm = TRUE) != 0), 
        ]
    logger(INFO, sprintf("Removed suspected bad dates \%s", setdiff(o.dates, 
        rownames(p))))
    if (!is.na(na.value)) {
        p[is.na(p)] <- 0
        logger(INFO, sprintf("Replaced NAs with \%s", na.value))
    }
    else {
        o.dates <- rownames(p)
        p <- p[apply(p, 1, function(x) sum(is.na(x)) < 0.1 * 
            length(x)), ]
        logger(INFO, sprintf("Removed dates with too many NAs \%s", 
            setdiff(o.dates, rownames(p))))
        nas <- apply(p, 2, function(x) !any(is.na(x)))
        p <- p[, which(nas == TRUE)]
        logger(INFO, sprintf("Removed symbols with NAs: \%s", 
            setdiff(symbols, anynames(p))))
    }
    if (is.null(obs)) {
        return(p[paste(start, end, sep = "::")])
    }
    p <- p[index(p) <= end]
    idx.inf <- anylength(p) - min(anylength(p), obs) + 1
    idx.sup <- anylength(p)
    logger(INFO, sprintf("Loaded portfolio with \%s assets", ncol(p)))
    out <- p[idx.inf:idx.sup, ]
    class(out) <- c(class(out), "returns")
    if (is.null(rownames(out))) 
        rownames(out) <- format(index(out), "\%Y-\%m-\%d")
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
