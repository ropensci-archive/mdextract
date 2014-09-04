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
#'lines(poly)
#'@export
points_to_poly <- function(pts, method = 'convex'){
  
  if (method != 'convex'){stop('method must be either convex or concave')}
  
  # if data.frame, convert to matrix...

  alpha_val <- get_alpha(pts)
  
  segments <- build_segs(pts, alpha_val)
  
  poly <- order_segments(segments)
  return(poly)
}

build_segs <- function(pts, alpha){
  orig_alpha <- alpha
  is_blob <- F
  step_m = 1.2 # growth parameter
  while (!is_blob){
    sp <- ashape(x=unique(pts), alpha = alpha)
    
    x1 <- sp$edges[, 3]
    y1 <- sp$edges[, 4]
    x2 <- sp$edges[, 5]
    y2 <- sp$edges[, 6]
    
    in1 <- sp$edges[, 1]
    in2 <- sp$edges[, 2]
    segments <- matrix(c(in1, in2, x1, y1, x2, y2), ncol = 6)
    is_blob <- is_blob(segments)
    alpha = alpha*step_m
  }
  return(segments)
}

is_blob <- function(segments){
  # false if any deadend vectors or multiple blobs
  solo = vector(length = nrow(segments))
  for (j in 1:nrow(segments)){
    i_1 <- segments[j,1]
    i_2 <- segments[j,2]
    t_1 <- segments[-j, 1]
    t_2 <- segments[-j, 2]
    if (sum(i_1 == t_1 | i_1 == t_2) != 1 | sum(i_2 == t_1 | i_2 == t_2) != 1){
      solo[j] <- T
    }
  }
  return(ifelse(all(!solo), T, F))
}
get_alpha <- function(pts){
  scl <- 5
  x_vals <- sort(unique(pts[, 1]))
  y_vals <- sort(unique(pts[, 2]))
  
  alpha = max(c(quantile(diff(x_vals),.95)[[1]], quantile(diff(y_vals),.95)[[1]]))*scl
  return(alpha)
}

segment_cleaner <- function(segments){
  clean_pile <- matrix(segments[1, ],ncol=6)
  segments = segments[-1, ]
  for (j in 1:(nrow(segments) -1)){
    # do we need to flip?
    u_i <- tail(clean_pile[, 2], 1)
    
    nxt_i <- which(u_i== segments[, 2])
    if (length(nxt_i) == 0){
      # no flip, add to clean
      nxt_i <- which(u_i== segments[, 1])
      clean_pile <- rbind(clean_pile, segments[nxt_i, ])
    } else {
      clean_pile <- rbind(clean_pile, segments[nxt_i, c(2,1,5,6,3,4)])
      
    }
    segments = matrix(segments[-nxt_i, ], ncol=6)
  }
  
  #last index
  if(nrow(segments) == 0){
    return(clean_pile)
  }
  if (segments[1] == clean_pile[1,1]){
    clean_pile <- rbind(clean_pile, segments[1,c(2,1,5,6,3,4)])
  } else {
    clean_pile <- rbind(clean_pile, segments)
  }
  return(clean_pile)
}

order_segments <- function(segments){
  # must close ring
  segments <- segment_cleaner(segments)

  ring <- rbind(segments[,3:4],segments[1,3:4]) # append to close ring
  return(ring)
  
}

