
declineIndexesPlot <- function(decline_indexes){
  decline_indexes %>%
    filter(ChosenGroup != 'Extra') %>%
    {
      .$ChosenGroup[.$ChosenGroup == 'Control'] <- 'Healthy'
      return(.)
    } %>%
    ggplot(aes(x = PDI,y = DAI)) +
    geom_point(aes(fill = ChosenGroup),shape = 21,size = 3) +
    scale_fill_manual(values = ptol_pal()(4)[c(4,1,2,3)]) +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold'),
          panel.border = element_blank(),
          axis.line = element_line(),
          panel.grid = element_blank()) +
    labs(fill = 'Assigned Decline Status')
}
