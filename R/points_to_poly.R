#'@title create polygon from series of points
#'
#'@details creates a automatically generated footprint from a series of points
#'
#'@param pts a matrix of latitute and longitude points, or a data.frame with 
#'\code{latitude} and \code{longitude} as column names
#'@param method a string specifying the shape of the polygon, defaults as 
#'\code{'convex'}. Currently \code{'convex'} is the only supported method.
#'@return a data.frame of latitude and longitude points representing the spatial footprint of the data
#'@keywords methods, math
#'@author Jordan S. Read
#'@import alphahull
#'@examples 
#'file_nm <- system.file(package='mdextract','ext','data_gbif_1.csv')
#'example_points <- read.table(file=file_nm,header=T,sep=',')
#'point_matrix <- matrix(as.numeric(c(example_points$decimalLongitude,example_points$decimalLatitude)),
#'  nrow=nrow(example_points),ncol=2)
#'plot(points_to_poly(point_matrix))
#'points(point_matrix)
#'@export
points_to_poly <- function(pts, method = 'convex'){
  
  if (method != 'convex'){stop('method must be either convex or concave')}
  
  # if data.frame, convert to matrix...

  sp <- ashape(x=unique(pts), alpha = 10)
  x <- c(sp$edges[,3], sp$edges[,5])
  y <- c(sp$edges[,4], sp$edges[,6])
  poly = matrix(c(x, y), ncol = 2)

    
  
  return(poly)
}