#'@title create polygon from series of points
#'
#'@details creates a automatically generated footprint from a series of points
#'
#'@param points a matrix of latitute and longitude points, or a data.frame with 
#'\code{latitude} and \code{longitude} as column names
#'@param shape a string specifying the shape of the polygon, defaults as 
#'\code{'convex'}. Can be either \code{'convex'} or \code{'concave'}
#'@return a data.frame of latitude and longitude points representing the spatial footprint of the data
#'@keywords methods, math
#'@author Jordan S. Read
#'@examples 
#'example_points <- read.table(file='exampple_data.tsv',header=T,sep='\t')
# point_matrix <- matrix(as.numeric(c(example_points[, 2],example_points[, 1])),nrow=nrow(example_points),ncol=2)
#'@export
points_to_poly <- function(points, shape = 'convex'){
  
  if (shape != 'convex' & shape != 'concave'){stop('shape must be either convex or concave')}
  
  return(points)
}