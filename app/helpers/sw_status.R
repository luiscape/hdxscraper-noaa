#
#  SCRAPERWIKI STATUS: ------------------------------------
#
#  Function to control the status of ScraperWiki's boxes.
#
#  --------------------------------------------------------
#
changeSwStatus <- function(type = NULL, message = NULL, verbose = F) {
  if (!is.null(message)) { content = paste("type=", type, "&message=", message, sep="") }
  else content = paste("type=", type, sep="")
  curlPerform(postfields = content, url = 'https://scraperwiki.com/api/status', post = 1L)
  
  if (verbose == T) {
    cat(content)
  }
}