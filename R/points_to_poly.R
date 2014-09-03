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
#'poly <- points_to_poly(point_matrix)
#'plot(point_matrix)
#'plot_poly(poly)
#'@export
points_to_poly <- function(pts, method = 'convex'){
  
  if (method != 'convex'){stop('method must be either convex or concave')}
  
  # if data.frame, convert to matrix...

  sp <- ashape(x=unique(pts), alpha = 10)
  ind_1 <- sort.int(sp$edges[, 1], index.return = T)$ix
  ind_2 <- sort.int(sp$edges[, 2], index.return = T)$ix
  
  x <- vector(length = length(sp$edges[, 1])*2)
  x[seq(1,length(x),2)] <- sp$edges[ind_2, 3]
  x[seq(2,length(x),2)] <- sp$edges[ind_2, 5]
  y <- x
  y[seq(1,length(x),2)] <- sp$edges[ind_2, 4]
  y[seq(2,length(x),2)] <- sp$edges[ind_2, 6]

  poly = matrix(c(x, y), ncol = 2)

    
  
  return(poly)
}
