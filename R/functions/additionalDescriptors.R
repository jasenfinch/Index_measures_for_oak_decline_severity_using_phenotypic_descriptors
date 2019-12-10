
crownCondition <- function(m,t) {
  (100 - m) * (1 - t/100)
}

crownVolume <- function(r,h,l,c) {
  (pi * r ^ 2 * c * (h - l)) / 200
}

bleedPrevalence <- function(a,A,b,B,d) {
  (a^2 * A + b^2 * B)/(3 * d * pi)
}

agrilusExitHoleDensity <- function(h,d) {
  h/((2 * d)/1000 * pi)
}
