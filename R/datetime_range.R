#' @title Automatically calculate start/end datetime range for your data 
#' @description Take a vector of datetimes and calculate the range
#' @param datetimes A vector of \code{POSIXt} or \code{date} 
#' @return A matrix of size nX2 representing the first and last datetimes. 
#' Type \quote{simple} always returns only one first/last pair.
#' @import mclust
#' @export
datetime_range<- function(datetimes, type="simple"){
  
  if(!inherits(datetimes,"POSIXt") & !inherits(datetimes, "Date")){
    stop("datetimes must be either of either Date or POSIXt class")
  }
  
  #Simple range
  if(tolower(type) == "simple"){
    
    time_range = range(datetimes)
    out = data.frame(starts=time_range[1], ends=time_range[2])
    return(out)
    
  }else if(tolower(type == "mclust")){
    #MClust option
    
    #Subtract median, Mclust doesn't like the large values for some reason
    clust_mod = Mclust(as.numeric(datetimes)-median(as.numeric(datetimes)),
                       modelNames="E")
    
    classes = clust_mod$classification
    u_classes = unique(classes)
    out = data.frame(starts=datetimes[1], ends=datetimes[1])
      
    for(i in 1:length(u_classes)){
      time_range = range(datetimes[classes == u_classes[i]])
      out[i,"starts"] = time_range[1]
      out[i,"ends"]   = time_range[2]
    }
    return(out)
    
  }else{
    stop("Type must be one of the following options {mclust, simple}")
  }
  
  
  return(range(datetimes))
  
}

