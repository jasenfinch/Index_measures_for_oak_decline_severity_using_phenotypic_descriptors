uk_map <- map_data('worldHires',region = 'UK:Great Britain')

site_locations <- tibble(
  site = c('Attingham','Big Wood','Chestnuts','Great Monks Wood','Hatchlands','Langdale','Richmond','Speculation','Winding Wood'),
  long = c(-2.668900,1.566249,-2.470558,0.644876,-0.463009,-2.308198, -0.270065,-2.563387,0.846158),
  lat = c(52.685301,52.298790,51.829905,51.898664,51.258740,52.086199,51.456043,51.818837,52.053536)
)

site_locations_map <- ggplot() +
  geom_polygon(data = uk_map,aes(x = long,y = lat,group = group),fill = NA,colour = 'dark grey') +
  geom_text_repel(data = site_locations,aes(x = long,y = lat,label = site),size = 3,nudge_y = 0.01,seed = 1234) + 
  geom_point(data = site_locations,aes(x = long,y = lat,fill = site),shape = 21) +
  scale_fill_ptol() +
  guides(fill = FALSE) +
  theme_void()
