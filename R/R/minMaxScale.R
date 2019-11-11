#' @export

minMaxScale <- function(vec){
  (vec - min(vec)) / (max(vec) - min(vec))
}
