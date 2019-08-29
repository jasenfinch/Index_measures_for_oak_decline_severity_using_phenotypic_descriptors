spectrumTrees_DAI <- list(
  `a)` = DIs$data[which(DIs$DIs$DAI == min(abs(DIs$DIs$DAI))),],
  `b)` = DIs$data[which(abs(DIs$DIs$DAI - 0.5) == min(abs(DIs$DIs$DAI - 0.5))),],
  `c)` = DIs$data[which(DIs$DIs$DAI == max(DIs$DIs$DAI)),],
  `d)` = DIs$data[which(DIs$DIs$DAI == min(abs(DIs$DIs$DAI))),],
  `e)` = DIs$data[which(abs(DIs$DIs$DAI + 0.5) == min(abs(DIs$DIs$DAI + 0.5))),],
  `f)` = DIs$data[which(DIs$DIs$DAI == min(DIs$DIs$DAI)),]
)

variables <- names(DIs$data)[map_lgl(DIs$data,is.numeric)]

ranges <- variables %>%
  map(~{
    DIs$data[,.] %>%
      unlist() %>%
      {tibble(min = min(.),
              max = max(.))} %>%
      mutate(center = min + (max - min)/2)
  }) %>%
  set_names(variables) %>%
  bind_rows(.id = 'Descriptor')

spectrumTrees_DAI$`a)`$`Active bleeds` <- ranges$min[ranges$Descriptor == 'Active bleeds']
spectrumTrees_DAI$`b)`$`Active bleeds` <- ranges$center[ranges$Descriptor == 'Active bleeds'] / 16
spectrumTrees_DAI$`c)`$`Active bleeds` <- ranges$max[ranges$Descriptor == 'Active bleeds'] / 16

spectrumTrees_DAI$`a)`$`Black staining` <- ranges$min[ranges$Descriptor == 'Black staining']
spectrumTrees_DAI$`b)`$`Black staining` <- ranges$center[ranges$Descriptor == 'Black staining'] / 16
spectrumTrees_DAI$`c)`$`Black staining` <- ranges$max[ranges$Descriptor == 'Black staining'] / 16

spectrumTrees_DAI$`d)`$`Crown radius (m)` <- ranges$max[ranges$Descriptor == 'Crown radius (m)']
spectrumTrees_DAI$`e)`$`Crown radius (m)` <- ranges$center[ranges$Descriptor == 'Crown radius (m)']
spectrumTrees_DAI$`f)`$`Crown radius (m)` <- ranges$min[ranges$Descriptor == 'Crown radius (m)']

spectrumTrees_DAI$`d)`$`Total height (m)` <- ranges$max[ranges$Descriptor == 'Total height (m)']
spectrumTrees_DAI$`e)`$`Total height (m)` <- ranges$center[ranges$Descriptor == 'Total height (m)']
spectrumTrees_DAI$`f)`$`Total height (m)` <- ranges$min[ranges$Descriptor == 'Total height (m)']

spectrumTrees_DAI$`d)`$`Lower crown height (sm)` <- ranges$max[ranges$Descriptor == 'Lower crown height (m)']
spectrumTrees_DAI$`e)`$`Lower crown height (m)` <- ranges$center[ranges$Descriptor == 'Lower crown height (m)']
spectrumTrees_DAI$`f)`$`Lower crown height (m)` <- ranges$min[ranges$Descriptor == 'Lower crown height (m)']

spectrumTrees_DAI$`a)`$`Diameter at breast height (cm)` <- ranges$min[ranges$Descriptor == 'Diameter at breast height (cm)']
spectrumTrees_DAI$`b)`$`Diameter at breast height (cm)` <- ranges$center[ranges$Descriptor == 'Diameter at breast height (cm)']
spectrumTrees_DAI$`c)`$`Diameter at breast height (cm)` <- ranges$max[ranges$Descriptor == 'Diameter at breast height (cm)']
spectrumTrees_DAI$`d)`$`Diameter at breast height (cm)` <- ranges$max[ranges$Descriptor == 'Diameter at breast height (cm)']
spectrumTrees_DAI$`e)`$`Diameter at breast height (cm)` <- ranges$center[ranges$Descriptor == 'Diameter at breast height (cm)']
spectrumTrees_DAI$`f)`$`Diameter at breast height (cm)` <- ranges$min[ranges$Descriptor == 'Diameter at breast height (cm)']

plotRanges_DAI_abc <- spectrumTrees_DAI[c('a)','b)','c)')] %>%
  names() %>%
  map(~{
    type <- .
    list(
      `Active bleed size (cm)` = seq(ranges$min[ranges$Descriptor == 'Active bleed size (cm)'],ranges$center[ranges$Descriptor == 'Active bleed size (cm)']/8,length.out = 100),
      `Black staining size (cm)` = seq(ranges$min[ranges$Descriptor == 'Black staining size (cm)'],ranges$center[ranges$Descriptor == 'Black staining size (cm)']/8,length.out = 100)
    ) %>%
      expand.grid() %>%
      as_tibble() %>%
      mutate(ID = 1) %>%
      left_join(spectrumTrees_DAI[[type]] %>%
                  select(-`Active bleed size (cm)`,-`Black staining size (cm)`) %>%
                  mutate(ID = 1),
                by = 'ID') %>%
      select(-ID) %>%
      mutate(
        `Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                               `Crown transparency (%)`),
        `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                           `Total height (m)`,
                                           `Lower crown height (m)`,
                                           `Crown condition (%)`),
        `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (cm)`,
                                                 `Active bleeds`,
                                                 `Black staining size (cm)`,
                                                 `Black staining`,
                                                 `Diameter at breast height (cm)`)
      ) %>%
      mutate(DAI = predict(DAIrf,newdata = .))
  }) %>%
  set_names(names(spectrumTrees_DAI[c('a)','b)','c)')]))

plotRanges_DAI_def <- spectrumTrees_DAI[c('d)','e)','f)')] %>%
  names() %>%
  map(~{
    type <- .
    list(
  `Crown transparency (%)` = seq(ranges$min[ranges$Descriptor == 'Crown transparency (%)'],ranges$max[ranges$Descriptor == 'Crown transparency (%)'],length.out = 100),
  `Missing crown (%)` = seq(ranges$min[ranges$Descriptor == 'Missing crown (%)'],ranges$max[ranges$Descriptor == 'Missing crown (%)'],length.out = 100)
) %>%
  expand.grid() %>%
  as_tibble() %>%
  mutate(ID = 1) %>%
  left_join(spectrumTrees_DAI[[type]] %>%
              select(-`Crown transparency (%)`,-`Missing crown (%)`) %>%
              mutate(ID = 1),
            by = 'ID') %>%
  select(-ID) %>%
  mutate(
    `Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                           `Crown transparency (%)`),
    `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                       `Total height (m)`,
                                       `Lower crown height (m)`,
                                       `Crown condition (%)`),
    `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (cm)`,
                                             `Active bleeds`,
                                             `Black staining size (cm)`,
                                             `Black staining`,
                                             `Diameter at breast height (cm)`)
  ) %>%
  mutate(DAI = predict(DAIrf,newdata = .))
  }) %>%
  set_names(names(spectrumTrees_DAI[c('d)','e)','f)')]))

DAI_response_plot_abc <- plotRanges_DAI_abc %>%
  names() %>%
  map(~{
    type <- .
    pl <- plotRanges_DAI_abc[[type]] %>%
      ggplot(aes(x = `Active bleed size (cm)`,y = `Black staining size (cm)`,fill = DAI,z = DAI)) +
      geom_raster() +
      geom_raster() +
      geom_contour(colour = 'black',binwidth = 0.1) +
      geom_text_contour(stroke = 0.2,binwidth = 0.1,min.size = 10,size = 3) +
      scale_fill_gradient2(low = 'blue',mid = 'green',high = 'red',midpoint = 0,limits = c(-1,1)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(face = 'bold',size = 9),
            axis.title = element_text(face = 'bold',size = 9),
            legend.title = element_text(face = 'bold')) +
      coord_fixed() +
      labs(title = type,
           caption = str_c(
             'Active bleeds = ',plotRanges_DAI_abc[[type]]$`Active bleeds`[1] %>% round(3),'\n',
             'Black staining = ',plotRanges_DAI_abc[[type]]$`Black staining`[1] %>% round(3),'\n',
             'Diameter at breast height (cm) = ',plotRanges_DAI_abc[[type]]$`Diameter at breast height (cm)`[1] %>% round(3)
           )
      )
    return(pl)
  }) %>%
  set_names(names(spectrumTrees_DAI[c('a)','b)','c)')]))

DAI_response_plot_def <- plotRanges_DAI_def %>%
  names() %>%
  map(~{
    type <- .
    pl <- plotRanges_DAI_def[[type]] %>%
  ggplot(aes(x = `Crown transparency (%)`,y = `Missing crown (%)`,fill = DAI,z = DAI)) +
      geom_raster() +
      geom_contour(colour = 'black',binwidth = 0.1) +
      geom_text_contour(stroke = 0.2,binwidth = 0.1,min.size = 10,size = 3) +
      scale_fill_gradient2(low = 'blue',mid = 'green',high = 'red',midpoint = 0,limits = c(-1,1)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(face = 'bold',size = 9),
            axis.title = element_text(face = 'bold',size = 9),
            legend.title = element_text(face = 'bold')) +
      coord_fixed() +
      labs(title = type,
           caption = str_c(
             'Crown radius (m) = ',plotRanges_DAI_def[[type]]$`Crown radius (m)`[1] %>% round(3),'\n',
             'Total height (m) = ',plotRanges_DAI_def[[type]]$`Total height (m)`[1] %>% round(3),'\n',
             'Lower crown height (m) = ',plotRanges_DAI_def[[type]]$`Lower crown height (m)`[1] %>% round(3),'\n',
             'Diameter at breast height (cm) = ',plotRanges_DAI_def[[type]]$`Diameter at breast height (cm)`[1] %>% round(3)
           )
      )
    return(pl)
  }) %>%
  set_names(names(spectrumTrees_DAI[c('d)','e)','f)')]))

DAI_legend <- get_legend(DAI_response_plot_abc$`a)`)

DAI_response_plot_abc <- DAI_response_plot_abc %>%
  map(~{
    . + guides(fill = FALSE)
  })

DAI_response_plot_def <- DAI_response_plot_def %>%
  map(~{
    . + guides(fill = FALSE)
  })

DAI_response_plot <- c(DAI_response_plot_abc,DAI_response_plot_def,list(legend = DAI_legend))
