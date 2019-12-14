
descriptorImportancePlots <- function(PDIrf,DAIrf){
  list(
    a = {
      dat <- PDIrf %>%
        {
          imp <- importance(.)
          feat <- rownames(imp)
          tibble(Feature = feat,`%IncMSE` = imp[,'%IncMSE'],
                 IncNodePurity = imp[,'IncNodePurity'])
        } %>%
        arrange(`%IncMSE`) %>%
        mutate(Feature = factor(Feature,levels = Feature), 
               `Relative Importance` = `%IncMSE` / max(`%IncMSE`)) 
      
      descriptorLabels <- dat$Feature %>%
        as.character() %>%
      {c(.[1:(which(str_detect(.,coll('Agrilus exit hole density (m^-2)'))) - 1)],
         expression(Agrillus~exit~hole~density ( m^-2 ) ),
         .[(which(str_detect(.,coll('Agrilus exit hole density (m^-2)'))) + 1):36],
         expression(Crown~volume ( m^3 ) ),
         expression(Crown~surface~area ( m^2 ))
      )}
      
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
      dat <- DAIrf %>%
        {
          imp <- importance(.)
          feat <- rownames(imp)
          tibble(Feature = feat,`%IncMSE` = imp[,'%IncMSE'],
                 IncNodePurity = imp[,'IncNodePurity'])
        } %>%
        arrange(`%IncMSE`) %>%
        mutate(Feature = factor(Feature,levels = Feature), 
               `Relative Importance` = `%IncMSE` / max(`%IncMSE`)) 
      
      descriptorLabels <- dat$Feature %>%
        as.character() %>%
        {c(.[1:(which(str_detect(.,coll('Agrilus exit hole density (m^-2)'))) - 1)],
           expression(Agrillus~exit~hole~density ( m^-2 ) ),
           .[(which(str_detect(.,coll('Agrilus exit hole density (m^-2)'))) + 1):(which(str_detect(.,coll('Crown surface area (m^2)'))) - 1)],
           expression(Crown~surface~area ( m^2 )),
           expression(Crown~volume ( m^3 ) )
        )}
      
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
