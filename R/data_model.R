#' Create an S4 class for coverages
#' @slot name the name of the dataframe that the coverage was generated from
#' @slot spatial a list containing all the spatial coverage data that was extracted.  This will be a bounding box or a gpolygon
#' @slot temporal a list of extracted temporal coverage data, usually just a begin and end date, but could be multiple start and end dates.
#' @slot taxanomic a list containing the taxanomic coverage metadata
#'

setClass("coverage", slots = c(name = "character",
                               spatial = "list",
                               temporal = "list",
                               taxanomic = "list",
                               data = "data.frame"
                                
))

setGeneric("coverage",function(name,spatial,temporal,taxanomic) {
  standardGeneric("coverage")
})

### Define constructor

setMethod("coverage", signature(), function(){
  cov <- new("coverage",name = "", spatial = list(),temporal=list(),taxanomic=list(),data=data.frame())
  
  return(cov)
})


#' @title Extract coverages from a dataset.
#' @description Extract temporal, spatial and taxanomic coverages from a dataset
#' @param df The dataset to extract coverages from, this should be a dataframe.
#' @param spatial a list of control parameters for extracting spatial coverages, see details for a full description
#' @details Eeach coverage needs a corresponding list of parameters that specificy where the extractor can look for the appropriate data.  
#' \itemize{ 
#' \item {spatial} {a list that contains three control parameters}}
#' @docType methods
#' @name coverage_extract-method
#' @rdname coverage_extract-method
#' @return Returns a coverage object with all coverages for a given dataset.
#' @examples \dontrun{
#' 
#' Examp goes here
#' }
#' @export
#' @import maps
#' @import mapdata
#' @import dplyr

setGeneric("coverage_extract", function(df,spatial = NULL, temporal = NULL, taxanomxic = NULL) {
  standardGeneric("coverage_extract")
})


#' @rdname coverage_extract-method
#' @aliases coverage_extract
#' @export

setMethod("coverage_extract",signature("data.frame"),function(df,spatial,temporal){
  # Generate new coverage with empty slots
  out <- coverage()
  out@data <- df
  if(!is.null(spatial)){
  ### Get just points for processing spatial data
  tmat <- cbind(df[[spatial$lat]],df[[spatial$lon]])
  ### check if elevation is desired
  if(spatial$elevation){
    ele <- elevation(tmat)
  } else {ele = NULL}
  
  
  if(spatial$type == "box"){
    bbox <- bounding_box(tmat)
    blist <- list(type = spatial$type,bbox = bbox, elevation = ele)
    out@spatial <- blist
    
   } else if(spatial$type == "poly"){
    poly <- points_to_poly(tmat)
    
    polylist <- list(type = spatial$type, poly = poly, elevation = ele)
    out@spatial <- polylist
  }
  
}


  if(!is.null(temporal)){
      if(temporal$type == "simple" && is.null(temporal$group_by)){
        date_df <- datetime_range(df[[temporal$date]])
        out@temporal <- list(start=date_df$starts,end = date_df$ends)        
      } else if(temporal$type=="clust" && is.null(temporal$group_by)){
        date_df <- datetime_range(df[[temporal$date]],type="mclust")
        out@temporal <- list(start=date_df$starts,end = date_df$ends)    
      }
      
    if(!is.null(group_by)){
      date_df <- cbind(df[[temporal$date]],df[[temporal$group_by]])
      
    }
      
      
  }

  return(out)
})




setMethod("plot",list(x = "coverage"),  function(x,type="spatial",...){
  if(type == "spatial"){
    if(x@spatial$type == "box"){
      map("worldHires")
      rect(x@spatial$bbox[1], x@spatial$bbox[3], x@spatial$bbox[2], x@spatial$bbox[4],lty=2,lwd=3)
      
    }
    if(x@spatial$type == "poly"){
      map("worldHires")
      lines(x@spatial$poly[,2:1],lty=2,lwd=3)
      
    }
  }
})