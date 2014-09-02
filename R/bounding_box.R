#' automatically generate coverages for a spocc search
#' @description This function will automatically generate metadata for spocc queries that can then be converted to other standards.
#' @param lonlat a 2 x n matrix of longitude-latitude coordinates
#' @export
bounding_box <- function(lonlat){
  spatial<- list()
  spatial$polygon <- "Bounding box"
    coord <- rbind(apply(out[,2:3],2,min),apply(out[,2:3],2,max))
    spatial$coord$UR <- coord[2,]
    spatial$coord$LL <- coord[1,]
    spatial$coord$UL <- c(coord[1,1],coord[2,2])
    spatial$coord$LR <- c(coord[2,1],coord[1,2])
  return(spatial)
}
