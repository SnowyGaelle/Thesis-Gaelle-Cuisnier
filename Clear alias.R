clearall <- function() {
  ENV <- globalenv()
  ll <- ls(envir = ENV)
  ll <- ll[ll != "clearall"]
  rm(list = ll, envir = ENV)
}
