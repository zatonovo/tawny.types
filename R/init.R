.onAttach <- function(libname,pkgname)
{
  .init()
}

.init <- function(loglevel=INFO)
{
  config_logger(threshold=loglevel)
  if (!exists('logger')) logger <<- getLogger('tawny')
  if (!exists('tawny.options'))
    tawny.options <<- OptionsManager('tawny.options',
      defaults=list(use.plots=FALSE))
}

