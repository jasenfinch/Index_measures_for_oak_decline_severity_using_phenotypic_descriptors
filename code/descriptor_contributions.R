
descriptor_contributions <- list(
  a = PDIrf %>%
  {
    imp <- importance(.)
    feat <- rownames(imp)
    imp <- imp[,1]
    tibble(Feature = feat,`Gini Importance` = imp)
  } %>%
  arrange(`Gini Importance`) %>%
  mutate(Feature = factor(Feature,levels = Feature), `Relative Importance` = `Gini Importance` / max(`Gini Importance`)) %>%
  {ggplot(.,aes(x = Feature,y = `Relative Importance`)) +
      geom_point(colour = ptol_pal()(1)) +
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold',size = 9),
            axis.title = element_text(face = 'bold')) +
      labs(title = 'Descriptor importance\nfor PDI',
           y = 'Relative importance',
           x = 'Descriptor')},
  b = DAIrf %>%
    {
      imp <- importance(.)
      feat <- rownames(imp)
      imp <- imp[,1]
      tibble(Feature = feat,`Gini Importance` = imp)
    } %>%
  arrange(`Gini Importance`) %>%
  mutate(Feature = factor(Feature,levels = Feature), `Relative Importance` = `Gini Importance` / max(`Gini Importance`)) %>%
  {ggplot(.,aes(x = Feature,y = `Relative Importance`)) +
      geom_point(colour = ptol_pal()(1)) +
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold',size = 9),
            axis.title = element_text(face = 'bold')) +
      labs(title = 'Descriptor importance\nfor DAI',
           y = 'Relative importance',
           x = 'Descriptor')}
)
