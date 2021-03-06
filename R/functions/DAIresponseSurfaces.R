
DAIresponseSurfaces <- function(DAIrf,DAI_example_cases,site_corrected_analysis_suitable_data){
  
  DAI_example_cases <- DAI_example_cases %>%
    set_names(c('a)','b)','c)','d)','e)','f)'))
  
  variables <- names(site_corrected_analysis_suitable_data)[map_lgl(site_corrected_analysis_suitable_data,is.numeric)]
  
  ranges <- variables %>%
    map(~{
      site_corrected_analysis_suitable_data[,.] %>%
        unlist() %>%
        {tibble(min = min(.),
                max = max(.),
                center = median(.))} %>%
        mutate(center = min + (max - min)/2)
    }) %>%
    set_names(variables) %>%
    bind_rows(.id = 'Descriptor')
  
  plotRanges_DAI_abc <- DAI_example_cases[c('a)','b)','c)')] %>%
    names() %>%
    map(~{
      type <- .
      list(
        `Active bleed length (mm)` = seq(ranges$min[ranges$Descriptor == 'Active bleed length (mm)'],ranges$center[ranges$Descriptor == 'Active bleed length (mm)']/8,length.out = 100),
        `Black staining length (mm)` = seq(ranges$min[ranges$Descriptor == 'Black staining length (mm)'],ranges$center[ranges$Descriptor == 'Black staining length (mm)']/8,length.out = 100)
      ) %>%
        expand.grid() %>%
        as_tibble() %>%
        mutate(ID = 1) %>%
        left_join(DAI_example_cases[[type]] %>%
                    select(-`Active bleed length (mm)`,-`Black staining length (mm)`) %>%
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
          `Bleed prevalence (%)` = bleedPrevalence(`Active bleed length (mm)`,
                                                   `Active bleeds`,
                                                   `Black staining length (mm)`,
                                                   `Black staining`,
                                                   `Diameter at breast height (m)`)
        ) %>%
        mutate(DAI = predict(DAIrf,newdata = .))
    }) %>%
    set_names(names(DAI_example_cases[c('a)','b)','c)')]))
  
  plotRanges_DAI_def <- DAI_example_cases[c('d)','e)','f)')] %>%
    names() %>%
    map(~{
      type <- .
      list(
        `Total height (m)` = seq(ranges$max[ranges$Descriptor == 'Lower crown height (m)'],ranges$max[ranges$Descriptor == 'Total height (m)'],length.out = 100),
        `Lower crown height (m)` = seq(ranges$min[ranges$Descriptor == 'Lower crown height (m)'],ranges$max[ranges$Descriptor == 'Lower crown height (m)'],length.out = 100)
      ) %>%
        expand.grid() %>%
        as_tibble() %>%
        mutate(ID = 1) %>%
        left_join(DAI_example_cases[[type]] %>%
                    select(-`Total height (m)`,-`Lower crown height (m)`) %>%
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
          `Bleed prevalence (%)` = bleedPrevalence(`Active bleed length (mm)`,
                                                   `Active bleeds`,
                                                   `Black staining length (mm)`,
                                                   `Black staining`,
                                                   `Diameter at breast height (m)`)
        ) %>%
        mutate(DAI = predict(DAIrf,newdata = .))
    }) %>%
    set_names(names(DAI_example_cases[c('d)','e)','f)')]))
  
  DAI_response_plot_abc <- plotRanges_DAI_abc %>%
    names() %>%
    map(~{
      type <- .
      pl <- plotRanges_DAI_abc[[type]] %>%
        ggplot(aes(x = `Active bleed length (mm)`,y = `Black staining length (mm)`,fill = DAI,z = DAI)) +
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
               'Diameter at breast height (m) = ',plotRanges_DAI_abc[[type]]$`Diameter at breast height (m)`[1] %>% round(3)
             )
        )
      return(pl)
    }) %>%
    set_names(names(DAI_example_cases[c('a)','b)','c)')]))
  
  DAI_response_plot_def <- plotRanges_DAI_def %>%
    names() %>%
    map(~{
      type <- .
      pl <- plotRanges_DAI_def[[type]] %>%
        ggplot(aes(x = `Total height (m)`,y = `Lower crown height (m)`,fill = DAI,z = DAI)) +
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
               'Diameter at breast height (m) = ',plotRanges_DAI_def[[type]]$`Diameter at breast height (m)`[1] %>% round(3),'\n',
               'Missing crown (%) = ',plotRanges_DAI_def[[type]]$`Missing crown (%)`[1] %>% round(3),'\n',  
               'Crown transparency (%) = ',plotRanges_DAI_def[[type]]$`Crown transparency (%)`[1] %>% round(3)
             )
        )
      return(pl)
    }) %>%
    set_names(names(DAI_example_cases[c('d)','e)','f)')]))
  
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
  return(DAI_response_plot)
}
