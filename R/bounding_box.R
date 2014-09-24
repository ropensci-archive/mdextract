#' @title Automatically calculate a bounding box for your points
#' @description Take a 2 x n matrix of lat/lon coordinates and convert them to a bounding box
#' @param points a 2 x n matrix or dataframe of lat/long coordinates in decimal degrees
#' @return a vector of boundings points as specified by EML - A sequence of: westBoundingCoordinate, eastBoundingCoordinate, northBoundingCoordinate, southBoundingCoordinate 	
#' @examples
#' file_nm <- system.file(package='mdextract','ext','data_gbif_1.csv')
#' example_points <- read.csv(file=file_nm,header=T)
#' 
#' ex_points = matrix(c(example_points$decimalLatitude, example_points$decimalLongitude), ncol=2)
#' 
#' ex_bb = bounding_box(ex_points)
#' 
#' plot(ex_points[,2], ex_points[,1], xlim=c(-180, 180), ylim=c(-90, 90))
#' rect(ex_bb[1], ex_bb[3], ex_bb[2], ex_bb[4])
#' 
#' @export

bounding_box <- function(pts){
  
    coord <- rbind(apply(pts,2,min),apply(pts,2,max))
    return(c(coord[,2],coord[,1]))
    }
