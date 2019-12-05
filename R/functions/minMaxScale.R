#' minMaxScale
#' @description variable min-max scaling
 
minMaxScale <- function(vec){
  (vec - min(vec)) / (max(vec) - min(vec))
}
