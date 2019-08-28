
descriptor_contributions <- list(
  a = {
    dat <- PDIrf %>%
      {
        imp <- importance(.)
        feat <- rownames(imp)
        imp <- imp[,1]
        tibble(Feature = feat,`Gini Importance` = imp)
      } %>%
      arrange(`Gini Importance`) %>%
      mutate(Feature = factor(Feature,levels = Feature), `Relative Importance` = `Gini Importance` / max(`Gini Importance`)) 
    
    descriptorLabels <- dat$Feature %>%
      as.character() %>%
      {c(.[1:5],
         expression(Agrillus~exit~hole~density ( m^-2 ) ),
         .[7:33],
         expression(Crown~volume ( m^3 ) )
      )}
    
    ggplot(dat,aes(x = Feature,y = `Relative Importance`)) +
      geom_point(fill = ptol_pal()(1),shape = 21) +
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold',size = 10)) +
      labs(title = 'a)',
           y = 'Relative Gini\nimportance',
           x = NULL) +
      scale_x_discrete(labels = descriptorLabels)
  },
  b = {
    dat <- DAIrf %>%
    {
      imp <- importance(.)
      feat <- rownames(imp)
      imp <- imp[,1]
      tibble(Feature = feat,`Gini Importance` = imp)
    } %>%
    arrange(`Gini Importance`) %>%
    mutate(Feature = factor(Feature,levels = Feature), `Relative Importance` = `Gini Importance` / max(`Gini Importance`))
    
    descriptorLabels <- dat$Feature %>%
      as.character() %>%
      {c(.[1:10],
         expression(Agrillus~exit~hole~density ( m^-2 ) ),
         .[12:30],
         expression(Crown~volume ( m^3 ) ),
         .[32:34]
      )}
    
    ggplot(dat,aes(x = Feature,y = `Relative Importance`)) +
        geom_point(fill = ptol_pal()(1),shape = 21) +
        coord_flip() +
        theme_bw() +
        theme(plot.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold',size = 10)) +
        labs(title = 'b)',
             y = 'Relative Gini\nimportance',
             x = NULL) +
      scale_x_discrete(labels = descriptorLabels)
  }
)
