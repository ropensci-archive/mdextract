#print_data_model

print.coverage <- function(x, ...){
  
  cat("Coverage data has the following info:\n")
  cat("Spatial type:", x$spatial$type, "\n");
}