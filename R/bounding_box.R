#' @title Automatically calculate a bounding box for your points
#' @description Take a 2 x n matrix of lat/lon coordinates and convert them to a bounding box
#' @param points a 2 x n matrix or dataframe of lat/long coordinates in decimal degrees
#' @return a vector of boundings points as specified by EML - A sequence of: westBoundingCoordinate, eastBoundingCoordinate, northBoundingCoordinate, southBoundingCoordinate 	
#' @export

bounding_box <- function(pts){
  
    coord <- rbind(apply(pts,2,min),apply(pts,2,max))
    return(c(coord[,2],coord[,1]))
    }
