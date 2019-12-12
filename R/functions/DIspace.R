#' DIspace
#' @description Plot the 2 dimensional decline index continuum.

DIspace <- function(){
  labels <- tibble(
    label = c('Healthy','Acute\nDecline','Chronic\nDecline','Declining','Severe\nDecline','Early\nAcute Decline'),
    x = c(0.17,0.75,0.66,0.5,0.83,0.25),
    y = c(0,0.66,-0.66,0,0,0.66)
  )
  
  segs <- 3000
  rectangle <- map(0:segs,~{
    f <- . /segs
    if (f < 0.5) {
      yend <- f * -1.34 - 0.33
      # yend <- f * -2 - 0
    } else {
      yend = -1
    }
    tibble(
      x = f,
      xend = f,
      y = 1,
      yend = yend
    )
  }) %>%
    bind_rows()
  
  hlines <- tibble(
    x = c(0.33,1,0,1),
    y = c(-0.33,-0.33,0.33,0.33),
    groups = c(1,1,2,2)
  )
  
  vlines <- tibble(
    x = c(0.33,0.33,0.5,0.5,0.66,0.66),
    y = c(0.33 * -1.34 - 0.33,0.33,0.33,1,-0.33,0.33),
    groups = c(1,1,2,2,3,3)
  )
  
  ggplot() +
    geom_segment(data = rectangle,aes(x = x,xend = xend,y = y,yend = yend,colour = x)) +
    scale_colour_gradient2(low = 'green',mid = 'orange',high = 'red',midpoint = 0.5) +
    scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1,1), expand = c(0, 0)) +
    geom_line(data = hlines,aes(x = x,y = y,group = groups),linetype = 2,colour = 'white') +
    geom_line(data = vlines,aes(x = x,y = y,group = groups),linetype = 2,colour = 'white') +
    geom_text(data = labels,aes(x = x,y = y,label = label)) +
    labs(x = 'Phenotypic Decline Index (PDI)',
         y = 'Decline Acuteness Index (DAI)') +
    theme_classic() +
    theme(plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold'),
          panel.background = element_rect(fill = '#302F2C')) +
    guides(colour = FALSE)
}