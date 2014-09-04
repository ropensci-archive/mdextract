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

  sp <- ashape(x=unique(pts), alpha = 10)
  
  x1 <- sp$edges[, 3]
  y1 <- sp$edges[, 4]
  x2 <- sp$edges[, 5]
  y2 <- sp$edges[, 6]
  
  in1 <- sp$edges[, 1]
  in2 <- sp$edges[, 2]
  segments <- matrix(c(in1, in2, x1, y1, x2, y2), ncol = 6)
  
  poly <- order_segments(segments)
  return(poly)
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
    clean_pile <- rbind(clean_pile, segments[c(2,1,5,6,3,4)])
  } else {
    clean_pile <- rbind(clean_pile, matrix(segments, ncol=6))
  }
  return(clean_pile)
}

order_segments <- function(segments){
  # must close ring
  segments <- segment_cleaner(segments)
  ring <- matrix(nrow = nrow(segments)*2+1, ncol = 2)
  ring[1, 1:2] <- segments[1, 3:4]
  r_cnt <- 2
  prev_match <- segments[1, 2] # the opposite index match
  prev_i <- 1
  for (j in 1:nrow(segments)){
    matches <- which(prev_match == segments[, 2])
    if (length(matches) == 0){
      
      # flip!
      cat('flip')
      matches <- which(prev_match == segments[, 1])
      match_i <- matches[matches!= prev_i]
      ring[r_cnt, 1:2] <- segments[match_i, 3:4]
      prev_i <- match_i
      prev_match <- segments[prev_i, 2]
    } else {
      match_i <- ifelse(length(matches) == 1, which(prev_match == segments[, 1]), matches[matches!= prev_i])
      ring[r_cnt, 1:2] <- segments[match_i, 5:6]
      prev_i <- match_i
      prev_match <- segments[prev_i, 1]
    }
    
    r_cnt <- r_cnt + 1
    
    # other side
    matches <- which(prev_match == segments[, 1])
    if (length(matches) == 1){
      # switch sides!
      match_i <- which(prev_match == segments[, 2])
      prev_i <- match_i
      ring[r_cnt, 1:2] <- segments[match_i, 5:6]
      prev_match <- segments[prev_i, 1]
    } else if (length(matches) == 0){
      # switch sides!
      matches <- which(prev_match == segments[, 2])
      match_i <- matches[matches!= prev_i]
      prev_i <- match_i
      ring[r_cnt, 1:2] <- segments[match_i, 5:6]
      prev_match <- segments[prev_i, 1]
    } else{
      match_i <- matches[matches!= prev_i]
      prev_i <- match_i
      ring[r_cnt, 1:2] <- segments[match_i, 3:4]
      prev_match <- segments[prev_i, 2]
    }
    r_cnt <- r_cnt + 1
  }
  ring <- ring[!is.na(ring[, 1]), ]
 
  return(ring)
}

