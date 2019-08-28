
locationRF <- rf(makeAnalysisTable(phenoData),
                 phenoData$Location %>%
                   factor(),
                 100)

locationMDS <- mds(locationRF) %>%
  bind_cols(phenoData %>%
              select(Location))

locationMargin <- margins(locationRF) %>%
  summarise(Margin = mean(Margin)) %>%
  mutate(x = 0.4,y = 0.5,label = str_c('Margin = ',Margin %>% round(2)))

locationImportance <- locationRF %>%
  map(~{
    imp <- importance(.)
    desc <- rownames(imp)
    imp <- imp %>%
      as_tibble() %>%
      mutate(Descriptor = desc) %>%
      select(Descriptor,MeanDecreaseGini)
    }) %>%
  bind_rows(.id = 'Rep') %>%
  group_by(Descriptor) %>%
  summarise(Importance = mean(MeanDecreaseGini)) %>%
  mutate(`Relative Importance` = Importance / max(Importance)) %>%
  arrange(`Relative Importance`) %>%
  mutate(Descriptor = factor(Descriptor,levels = Descriptor))

descriptorLabels <- locationImportance$Descriptor %>%
  as.character() %>%
  {c(.[1:6],
                      expression(Agrillus~exit~hole~density ( m^-2 ) ),
                      .[8:25],
                      expression(Crown~volume ( m^3 ) ),
                      .[27:34])
  }

site_differences <- list(
  a = ggplot() +
    geom_point(data = locationMDS,aes(x = `Dimension 1`, y = `Dimension 2`,fill = Location),shape = 21,size = 3) +
    geom_text(data = locationMargin,aes(x = x,y = y,label = label)) +
    scale_fill_ptol() +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold'),
          legend.position = 'bottom') +
    coord_fixed() +
    guides(fill = guide_legend(ncol = 2,title.position = "top")) +
    labs(title = 'a)',
         fill = 'Site'),
  b = ggplot(locationImportance,aes(x = `Relative Importance`,y = Descriptor)) +
    geom_point(shape = 21,fill = ptol_pal()(1),size = 3) +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold',size = 10)) +
    labs(title = 'b)',
         y = NULL,
         x = 'Relative Gini\nimportance') +
    scale_y_discrete(labels = descriptorLabels)
)
