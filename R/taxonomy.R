#' This function creates a taxonomic coverage data object.
#'
#' @export
#' @import taxize
#' @examples \dontrun{
#' datpath <- system.file("inst/ext/data_gbif_1.csv", package = "mdextract")
#' dat <- read.csv(datpath)
#' taxonomy_handler(input=dat, fields=c('name'))
#' }

taxonomy_handler <- function(input, genus_epithet=NULL, genus=NULL, epithet=NULL, subspecies=NULL)
{  
  if(is.null(fields) || !is.character(fields)) stop("Please supply fields to use for taxonomy coverage")
  
  out <- 'XX'
  class(out) <- 'coverage_taxonomy'
  return(out)
}
