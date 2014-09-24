#' Create an S4 class for coverages
#' @slot name the name of the dataframe that the coverage was generated from
#' @slot spatial a list containing all the spatial coverage data that was extracted.  This will be a bounding box or a gpolygon
#' @slot temporal a list of extracted temporal coverage data, usually just a begin and end date, but could be multiple start and end dates.
#' @slot taxanomic a list containing the taxanomic coverage metadata
#'

setClass("Coverage", slots = c(name = "character",
                               spatial = "list",
                               temporal = "list",
                               taxanomic = "list"
                                
))

setGeneric("coverage",function(name,spatial,temporal,taxanomic) {
  standardGeneric("coverage")
})

### Define constructor

setMethod("coverage", signature(), function(){
  cov <- new("Coverage",name = covName, spatial = list(),temporal=list(),taxanomic=list())
  
  return(cov)
})



#' list formats
#' @description list of all object formats registered in the DataONE Object Format Vocabulary.
#' @param cnode a valid CNode object
#' @docType methods
#' @author hart
#' @return Returns a dataframe of all object formats registered in the DataONE Object Format Vocabulary.
#' @examples
#' \dontrun {
#' cn <- CNode()
#' listFormats(cn)
#' }
#' @export
setGeneric("listFormats", function(cnode, ...) {
  standardGeneric("listFormats")
})


#' @rdname listFormats-method
#' @aliases listFormats
#' @export


