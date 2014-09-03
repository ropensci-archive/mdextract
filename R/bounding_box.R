#' @title Automatically calculate a bounding box for your points
#' @description Take a 2 x n matrix of lat/lon coordinates and convert them to a bounding box
#' @param points a 2 x n matrix or dataframe of lat/long coordinates
#' @return a vector of boundings points as specified by EML - A sequence of: westBoundingCoordinate, eastBoundingCoordinate, northBoundingCoordinate, southBoundingCoordinate 	
#' @export

bounding_box <- function(points){
  
    coord <- rbind(apply(points,2,min),apply(poinst,2,max))
    
    
  return(spatial)
}
