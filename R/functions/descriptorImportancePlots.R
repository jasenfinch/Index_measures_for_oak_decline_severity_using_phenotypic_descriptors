
descriptorImportancePlots <- function(PDI_descriptor_importance,DAI_descriptor_importance){
  importance_plots <- list(
    a = {
      dat <- PDI_descriptor_importance %>%
        arrange(`%IncMSE`) %>%
        mutate(Feature = factor(Feature,levels = Feature),
               rev_Rank = Feature %>%
                 seq_along())
      descriptorLabels <- dat$Feature %>%
        as.character() %>%
        {
          .[str_detect(.,coll('Agrilus exit hole density (m^-2)'))] <- expression(Agrillus~exit~hole~density ( m^-2 ) )
          .[str_detect(.,coll('Crown volume (m^3)'))] <- expression(Crown~volume ( m^3 ) )
          return(.)
        }
      
      ggplot(dat,aes(y = Feature,x = `%IncMSE`)) +
        geom_segment(aes(y = rev_Rank,yend = rev_Rank,x = -3,xend = `%IncMSE`)) +
        geom_point(fill = ptol_pal()(1),shape = 21,size = 2) +
        theme_bw() +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5),
              axis.title = element_text(face = 'bold',size = 10),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt"),
              axis.line.x = element_line(),
              axis.text.y = element_text(colour = 'black'),
              axis.ticks.y = element_blank()) +
        labs(title = 'a) PDI',
             x = '% increase in\nMSE',
             y = NULL) +
        scale_y_discrete(labels = descriptorLabels)  +
        scale_x_reverse(limits = c(112,-5),
                        expand = c(0,0))
    },
    b = {
      dat <- DAI_descriptor_importance %>%
        arrange(`%IncMSE`) %>%
        mutate(Feature = factor(Feature,levels = Feature),
               rev_Rank = Feature %>%
                 seq_along())
      
      descriptorLabels <- dat$Feature %>%
        as.character() %>%
        {
          .[str_detect(.,coll('Agrilus exit hole density (m^-2)'))] <- expression(Agrillus~exit~hole~density ( m^-2 ) )
          .[str_detect(.,coll('Crown volume (m^3)'))] <- expression(Crown~volume ( m^3 ) )
          return(.)
        }
      
      ggplot(dat,aes(y = Feature,x = `%IncMSE`)) +
        geom_segment(aes(y = rev_Rank,yend = rev_Rank,x = -3,xend = `%IncMSE`)) +
        geom_point(fill = ptol_pal()(1),shape = 21,size = 2) +
        theme_bw() +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5),
              axis.title = element_text(face = 'bold',size = 10),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              plot.margin = unit(c(5.5, 5.5, 5.5, 0), "pt"),
              axis.line.x = element_line(),
              axis.text.y = element_text(colour = 'black'),
              axis.ticks.y = element_blank()) +
        labs(title = 'b) DAI',
             x = '% increase in\nMSE',
             y = NULL) +
        scale_y_discrete(labels = descriptorLabels,
                         position = 'right') +
        scale_x_continuous(limits = c(-5,112),
                           expand = c(0,0))
    }
  )
  
  rank_links <- PDI_descriptor_importance %>%
    arrange(`%IncMSE`) %>%
    mutate(Feature = factor(Feature,levels = Feature),
           PDI_Rank = Feature %>%
             seq_along()) %>%
    select(-`%IncMSE`,-IncNodePurity) %>%
    left_join(DAI_descriptor_importance %>%
                arrange(`%IncMSE`) %>%
                mutate(Feature = factor(Feature,levels = Feature),
                       DAI_Rank = Feature %>%
                         seq_along()) %>%
                select(-`%IncMSE`,-IncNodePurity), 
              by = "Feature") %>%
    ggplot() +
    geom_segment(x = 0,
                 xend = 0,
                 y = 1,yend = 36) +
    geom_segment(x = 1,
                 xend = 1,
                 y = 1,yend = 36) +
    geom_segment(aes(y = PDI_Rank,
                     yend = DAI_Rank,
                     x = 0,
                     xend = 1),
                 linetype = 5) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.margin = unit(c(5.5, 0, 5.5, 0), "pt")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(labels = descriptorLabels)
  
  wrap_plots(importance_plots$a,rank_links,importance_plots$b,widths = c(2,1,2))   
}
