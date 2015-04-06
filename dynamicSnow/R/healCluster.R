#' healCluster
#' 
#' Repeatedly checks the status of nodes, disables the first inactive node,
#' transitions the identity of the last active node to the first inactive node
#' then disables the identity of the last active node so that it only
#' exists once in the cluster list.
#' 
#' @param cl The cluster object
#' @return cl The healed cluster object
#' @export
healCluster <- function(cl) {
  status <- checkPulse(cl)
  while (!all(status)) {
    topHealthy <- max(which(status))
    bottomUnhealthy <- min(which(!status))
    cl[[bottomUnhealthy]] <- cl[[topHealthy]]
    cl[[bottomUnhealthy]]$rank <- bottomUnhealthy
    cl[[topHealthy]] <- NULL    
    status <- checkPulse(cl)
  }
  return(cl)
}
NULL