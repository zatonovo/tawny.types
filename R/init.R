.onLoad <- function(a,b)
{
  require(futile.paradigm)
  require(futile.logger)
  require(quantmod)
  .init()
}

.init <- function()
{
  config_logger()
  if (!exists('logger')) logger <<- getLogger('tawny')
  if (!exists('tawny.options'))
    tawny.options <<- OptionsManager('tawny.options',
      defaults=list(use.plots=FALSE))
}

