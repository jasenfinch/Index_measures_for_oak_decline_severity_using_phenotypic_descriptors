#' @export

crownCondition <- function(m,t) {
  (100 - m) * (1 - t/100)
}

#' @export

crownVolume <- function(r,h,l,c) {
  (c * (h - l))/150 * pi * r^2
}

#' @export

bleedPrevalence <- function(a,A,b,B,d) {
  (a^2 * A + b^2 * B)/(3 * d * pi)
}

#' @export

agrilusExitHoleDensity <- function(h,d) {
  h/((2 * d)/100 * pi)
}
