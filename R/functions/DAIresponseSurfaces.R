
DAIresponseSurfaces <- function(DAIrf,decline_indexes,site_corrected_analysis_suitable_data){
  spectrumTrees_DAI <- list(
    `a)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI) == min(abs(decline_indexes$DAI))),],
    `b)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI - 0.5) == min(abs(decline_indexes$DAI - 0.5))),],
    `c)` = site_corrected_analysis_suitable_data[which(decline_indexes$DAI == max(decline_indexes$DAI)),],
    `d)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI) == min(abs(decline_indexes$DAI))),],
    `e)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI + 0.5) == min(abs(decline_indexes$DAI + 0.5))),],
    `f)` = site_corrected_analysis_suitable_data[which(decline_indexes$DAI == min(decline_indexes$DAI)),]
  )
  
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
  
  plotRanges_DAI_abc <- spectrumTrees_DAI[c('a)','b)','c)')] %>%
    names() %>%
    map(~{
      type <- .
      list(
        `Active bleed size (mm)` = seq(ranges$min[ranges$Descriptor == 'Active bleed size (mm)'],ranges$center[ranges$Descriptor == 'Active bleed size (mm)']/8,length.out = 100),
        `Black staining size (mm)` = seq(ranges$min[ranges$Descriptor == 'Black staining size (mm)'],ranges$center[ranges$Descriptor == 'Black staining size (mm)']/8,length.out = 100)
      ) %>%
        expand.grid() %>%
        as_tibble() %>%
        mutate(ID = 1) %>%
        left_join(spectrumTrees_DAI[[type]] %>%
                    select(-`Active bleed size (mm)`,-`Black staining size (mm)`) %>%
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
          `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (mm)`,
                                                   `Active bleeds`,
                                                   `Black staining size (mm)`,
                                                   `Black staining`,
                                                   `Diameter at breast height (mm)`)
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
          `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (mm)`,
                                                   `Active bleeds`,
                                                   `Black staining size (mm)`,
                                                   `Black staining`,
                                                   `Diameter at breast height (mm)`)
        ) %>%
        mutate(DAI = predict(DAIrf,newdata = .))
    }) %>%
    set_names(names(spectrumTrees_DAI[c('d)','e)','f)')]))
  
  DAI_response_plot_abc <- plotRanges_DAI_abc %>%
    names() %>%
    map(~{
      type <- .
      pl <- plotRanges_DAI_abc[[type]] %>%
        ggplot(aes(x = `Active bleed size (mm)`,y = `Black staining size (mm)`,fill = DAI,z = DAI)) +
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
               'Diameter at breast height (mm) = ',plotRanges_DAI_abc[[type]]$`Diameter at breast height (mm)`[1] %>% round(3)
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
               'Diameter at breast height (mm) = ',plotRanges_DAI_def[[type]]$`Diameter at breast height (mm)`[1] %>% round(3)
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
  return(DAI_response_plot)
}
