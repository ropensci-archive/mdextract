#' @title calculate elevation
#' @description given a matrix of points, figure out the min and max elevations of the points using the google elevation API
#' @param points a 2 x n matrix or dataframe of lat/lon coordinates in decimal degrees
#' @details Given the limited number of points that can be sent to the elevation API.  However too many points may cause the api to bork.
#' @return a vector that gives the minimum and maximum elevation in meters
#' @import httr


elevation <- function(pts){
  base <- "http://maps.googleapis.com/maps/api/elevation/json?locations="
  ## Create vector of lat/lon pairs
  llpairs <- apply(pts,function(x){out <- sapply(x,as.character); paste(out,collapse=",")})
  loops <- ceiling(length(llpairs) / 100)
  if(loops < 2){
    gpts <- paste(llpairs,collapse="|")
    url <- paste(base,gpts,sep="")
    res <- GET(url) 
    out <- content(res,as="parsed",type="application/json")
    elvec <- unlist(lapply(out$results,function(x){return(x$elevation)}))
  } else{
    g <- sort(rep(1:loops,100))
    #handle the remainder
    index <- 1:length(llpairs)
    g <- ind[index]
    elvec <- vector()
  for(i in 1:loops){
    gpts <- paste(llpairs[index[which(g==i)]],collapse="|")
    url <- paste(base,gpts,sep="")
    res <- GET(url) 
    if(res$status==400){stop("You may not be able to get elevations on all your points, consider using a subset for a best guess")}
    out <- content(res,as="parsed",type="application/json")
    elvec <- c(elvec,unlist(lapply(out$results,function(x){return(x$elevation)})))
    }
  }
  minA <- ifelse(min(elvec,na.rm=T) < 0,0,min(elvec,na.rm=T))
  maxA <- max(elvec,na.rm=T)
  return(list(min = minA,max =maxA))
}



