
liveCrownRatio <- function(h,l){
  (h - l) / h * 100
}

crownSurfaceArea <- function(r,h,l,c){
  cl <- h - l
 ((4 * pi * cl) / (3 * r ^ 2)) * ((r ^ 2 + r ^ 4 / (4 * cl ^ 2)) ^ 1.5 - (r ^ 4 / (4 * cl ^ 2)) ^ 1.5) * c / 100
}

crownProductionEfficiency <- function(crown_surface_area,crown_volume){
  crown_surface_area / crown_volume
}