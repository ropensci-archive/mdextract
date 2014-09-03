#' @title Automatically calculate start/end datetime range for your data 
#' @description Take a vector of datetimes and calculate the range
#' @param datetimes A vector of \code{POSIXt} or \code{date} 
#' @return A vector of length 2 representing the first and last datetime
#' @export
datetime_range<- function(datetimes){
  
  if(inherits(datetimes, "Date")){
    datetimes = as.POSIXct(datetimes)
  }
  
  if(!inherits(datetimes,"POSIXt") | !inherits(datetimes, "Date")){
    stop("datetimes must be either of either Date or POSIXt class")
  }
  
  
  return(range(datetimes))
  
}

