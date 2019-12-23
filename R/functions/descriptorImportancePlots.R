
descriptorImportancePlots <- function(PDI_descriptor_importance,DAI_descriptor_importance){
  list(
    a = {
      dat <- PDI_descriptor_importance %>%
        arrange(`%IncMSE`) %>%
        mutate(Feature = factor(Feature,levels = Feature)) 
      descriptorLabels <- dat$Feature %>%
        as.character() %>%
        {
          .[str_detect(.,coll('Agrilus exit hole density (m^-2)'))] <- expression(Agrillus~exit~hole~density ( m^-2 ) )
          .[str_detect(.,coll('Crown volume (m^3)'))] <- expression(Crown~volume ( m^3 ) )
          return(.)
        }
      
      ggplot(dat,aes(x = Feature,y = `%IncMSE`)) +
        geom_point(fill = ptol_pal()(1),shape = 21,size = 2) +
        coord_flip() +
        theme_bw() +
        theme(plot.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold',size = 10)) +
        labs(title = 'a) PDI',
             y = 'Mean decrease in\naccuracy',
             x = NULL) +
        scale_x_discrete(labels = descriptorLabels)
    },
    b = {
      dat <- DAI_descriptor_importance %>%
        arrange(`%IncMSE`) %>%
        mutate(Feature = factor(Feature,levels = Feature)) 
      
      descriptorLabels <- dat$Feature %>%
        as.character() %>%
        {
          .[str_detect(.,coll('Agrilus exit hole density (m^-2)'))] <- expression(Agrillus~exit~hole~density ( m^-2 ) )
          .[str_detect(.,coll('Crown volume (m^3)'))] <- expression(Crown~volume ( m^3 ) )
          return(.)
        }
      
      ggplot(dat,aes(x = Feature,y = `%IncMSE`)) +
        geom_point(fill = ptol_pal()(1),shape = 21,size = 2) +
        coord_flip() +
        theme_bw() +
        theme(plot.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold',size = 10)) +
        labs(title = 'b) DAI',
             y = 'Mean decrease in\naccuracy',
             x = NULL) +
        scale_x_discrete(labels = descriptorLabels)
    }
  )
}
