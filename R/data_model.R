#' This function creates a coverage data object.
#'
#' The coverage object stores metadata created
#' by other functions in this package
#'
#' @title Coverage: The coverage data model
#' @examples
#' new_coverage <- coverage()
#' 
#' @export
coverage <- function(){
  
  out = list(spatial=NA, time=NA, taxanomic=NA)
  
  out$spatial = list(type=NA, data=NA)
  
  class(out) <- 'coverage'
  return(out)
}