#' Create an S4 class for coverages
#' @slot name the name of the dataframe that the coverage was generated from
#' @slot spatial a list containing all the spatial coverage data that was extracted.  This will be a bounding box or a gpolygon
#' @slot temporal a list of extracted temporal coverage data, usually just a begin and end date, but could be multiple start and end dates.
#' @slot taxanomic a list containing the taxanomic coverage metadata
#'

setClass("coverage", slots = c(name = "character",
                               spatial = "list",
                               temporal = "list",
                               taxanomic = "list"
                                
))

#' Constructor for coverage class
#' 
#' @export
#' @docType methods
#' @rdname coverage-methods

setGeneric("coverage",function(name,spatial,temporal,taxanomic) {
  standardGeneric("coverage")
})
  

setMethod("coverage", signature("character","list","list","list"),function(name = NULL,spatial=NULL,temporal=NULL,taxanomic=NULL){
  cov <- new("coverage",name = name, spatial = spatial,temporal=temporal,taxanomic=taxanomic)
  return(cov)
})


