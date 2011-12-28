############################## TAWNY_PORTFOLIO ###############################
#create.TawnyPortfolio <- function(...) UseFunction('create.TawnyPortfolio',...)

# Example
# p <- create(TawnyPortfolio, c('FCX','AAPL','JPM','AMZN','VMW','TLT','GLD','FXI','ILF','XOM'))
create.TawnyPortfolio %when% is.character(symbols)
create.TawnyPortfolio %as% function(T, symbols)
{
  create.TawnyPortfolio(T, symbols, 90, 150)
}

create.TawnyPortfolio %when% is.character(symbols)
create.TawnyPortfolio %as% function(T, symbols, window, obs)
{
  returns = create(AssetReturns, symbols, obs)
  create.TawnyPortfolio(T, returns, window)
}

create.TawnyPortfolio %when% (returns %isa% AssetReturns)
create.TawnyPortfolio %as% function(T, returns)
{
  create.TawnyPortfolio(T, returns, 90)
}

create.TawnyPortfolio %when% (returns %isa% zoo)
create.TawnyPortfolio %as% function(T, returns, window)
{
  class(returns) <- c("AssetReturns", class(returns), "returns")
  create.TawnyPortfolio(T, returns, window)
}

create.TawnyPortfolio %when% (returns %isa% AssetReturns)
create.TawnyPortfolio %as% function(T, returns, window)
{
  periods = anylength(returns) - window + 1
  list(symbols=anynames(returns), window=window, obs=anylength(returns),
       periods=periods, returns=returns)
}

rollapply.TawnyPortfolio <- function(p, fun=fun, ...)
{
  steps <- array(seq(1,p$periods))
  out <- apply(steps, 1, function(idx) fun(window_at(p,idx), ...))
  out <- t(out)
  rownames(out) <- format(index(p$returns[(p$obs - p$periods + 1):p$obs,]))
  out
}

window_at %when% (p %isa% TawnyPortfolio)
window_at %as% function(p, idx)
{
  returns <- p$returns[idx:(p$window + idx - 1),]
  p$returns <- returns
  p
}

start.TawnyPortfolio <- function(p, ...)
{
  start(p$returns[p$obs - p$periods,])
}

end.TawnyPortfolio <- function(p, ...)
{
  end(p$returns)
}

# This produces a portfolio in matrix format (t x m) as a zoo class. 
# Params
#  symbols: A vector of symbols to retrieve. This uses quantmod to retrieve
#    the data.
#  obs: The number of observations that you want. Use this if you want the 
#    number of points to be explicit. Either obs or start is required.
#  start: The start date, if you know that explicitly. Using this will ensure
#    that the data points are bound to the given range but the precise number
#    of points will be determined by the number of trading days.
#  end: The most recent date of observation. Defaults to current day.
#  fun: A function to use on each symbol time series. Defaults to Cl to operate
#    on close data. For expected behavior, your function should only return
#    one time series.
# TODO: 
#  Fix names
#  Add method to add other portfolio elements (such as synthetic securities)
# Example:
#  h <- create(AssetReturns, c('GOOG','AAPL','BAC','C','F','T'), 150)
create.AssetReturns <- function(T, symbols, obs=NULL,
  start=NULL, end=Sys.Date(),
  fun=function(x) Delt(Cl(x)), reload=FALSE, na.value=NA, ...)
{
  if (is.null(start) & is.null(obs)) { stop("Either obs or start must be set") }
  end <- as.Date(end)

  # Estimate calendar days from windowed business days. The 10 is there to
  # ensure enough points, which get trimmed later
  if (is.null(start)) { start <- end - (10 + obs * 365/250) }

  #ensure(symbols, src='yahoo', reload=reload, from=start, to=end, ...)

  # Merge into a single zoo object
  p <- xts(order.by=end)
  for (s in symbols)
  {
    asset <- getSymbols(s, from=start, to=end, auto.assign=FALSE)
    raw <- fun(asset)
    logger(INFO, sprintf("Binding %s for [%s,%s]",s, format(start(raw)),format(end(raw))))
      
    a <- xts(raw, order.by=index(asset))
    p <- cbind(p, a[2:anylength(a)])
  }
  colnames(p) <- symbols
  # First remove dates that have primarily NAs (probably bad data)
  o.dates <- rownames(p)
  p <- p[apply(p, 1, function(x) sum(x, na.rm=TRUE) != 0), ]
  logger(INFO, sprintf("Removed suspected bad dates %s",setdiff(o.dates,rownames(p))))

  if (! is.na(na.value))
  {
    #for (s in symbols) p[,s][is.na(p[,s])] <- na.value
    p[is.na(p)] <- 0
    logger(INFO, sprintf("Replaced NAs with %s",na.value))
  }
  else
  {
    # NOTE: This has consistency issues when comparing with a market index
    o.dates <- rownames(p)
    p <- p[apply(p, 1, function(x) sum(is.na(x)) < 0.1 * length(x) ), ]
    logger(INFO, sprintf("Removed dates with too many NAs %s",setdiff(o.dates,rownames(p))))

    # Now remove columns with NAs
    nas <- apply(p, 2, function(x) !any(is.na(x)) )
    p <- p[,which(nas == TRUE)]
    logger(INFO, sprintf("Removed symbols with NAs: %s",setdiff(symbols,anynames(p))))
  }

  if (is.null(obs)) { return(p[paste(start,end, sep='::')]) }

  p <- p[index(p) <= end]
  idx.inf <- anylength(p) - min(anylength(p), obs) + 1
  idx.sup <- anylength(p)
  
  logger(INFO, sprintf("Loaded portfolio with %s assets",ncol(p)))
  out <- p[idx.inf:idx.sup, ]
  class(out) <- c(class(out), 'returns')

  if (is.null(rownames(out))) rownames(out) <- format(index(out), "%Y-%m-%d")
  out
}

# Generate the composition for an equity index
# Example
# Get SP500 components
#   sp500.idx <- create(EquityIndex)
# Get DOW components
#   dow.idx <- create(EquityIndex,'^DJI')
# Get FTSE components
#   ftse.idx <- create(EquityIndex,'^FTSE')
# Get HSI components
#   hsi.idx <- create(EquityIndex,'^HSI')
# h <- create(AssetReturns, create(EquityIndex,'^DJI'), obs=100)
create.EquityIndex <- function(T, ticker='^GSPC', hint=NA, src='yahoo')
{
  if (is.na(hint))
  {
    hints <- c(500, 30, 102, 42)
    names(hints) <- c('^GSPC', '^DJI', '^FTSE', '^HSI')
    hint <- hints[ticker]
  }

  # http://download.finance.yahoo.com/d/quotes.csv?s=@%5EGSPC&f=sl1d1t1c1ohgv&e=.csv&h=0
  # TODO Fix the URL for the composition
  base <- 'http://download.finance.yahoo.com/d/quotes.csv?s=@'
  formats <- '&f=sl1d1t1c1ohgv&e=.csv&h='

  comp <- NULL
  pages = max(1, hint %/% 50)
  for (page in 1:pages)
  {
    start <- (page-1) * 50 + 1
    url <- paste(base, ticker, formats, start, sep='')
    logger(INFO, sprintf("Loading page %s for %s",page,ticker))
    data <- read.csv(url, header=FALSE)

    # This is here due to a bug in Yahoo's download where the first record gets
    # duplicated in each subsequent page
    idx = 2; if (page == 1) { idx = 1 }
    comp <- rbind(comp, data[idx:anylength(data),])

  }
  as.character(comp[,1])
}


