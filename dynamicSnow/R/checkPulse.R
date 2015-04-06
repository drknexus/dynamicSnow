#' checkPulse
#' 
#' Nudges each node to see if the connection is alive
#' 
#' @param cl The cluster object
#' @return A logical vector by list order of the nodes in the cluster object and whether they responded
#' @export
checkPulse <- function (cl = NULL) 
{
  #TODO:  use withTimeout from R.utils to handle hung nodes
  if (is.null(cl)) {
    stop("Cluster not explicitly specified")
  }
  
  #Convenently, print pokes just enough at the node to provoke an error if one is going to happen
  printTest <- function(x) {utils::capture.output(print(x));return(TRUE)}
  
  status <- rep(TRUE,length(cl))
  for (i in seq_along(cl)) {
    status[i] <- tryCatch(printTest(cl[[i]]), error=function(e) {FALSE})
  }
  return(status)
}
NULL