
PDIresponseSurfaces <- function(PDIrf,decline_indexes,site_corrected_analysis_suitable_data){
  spectrumTrees_PDI <- list(
    `a)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == min(decline_indexes$PDI)),],
    `b)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == min(decline_indexes$PDI)),],
    `c)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == min(decline_indexes$PDI)),]
  )
  
  variables <- names(site_corrected_analysis_suitable_data)[map_lgl(site_corrected_analysis_suitable_data,is.numeric)]
  
  ranges <- variables %>%
    map(~{
      site_corrected_analysis_suitable_data[,.] %>%
        unlist() %>%
        {tibble(min = min(.),
                max = max(.))} %>%
        mutate(center = min + (max - min)/2)
    }) %>%
    set_names(variables) %>%
    bind_rows(.id = 'Descriptor')
  
  spectrumTrees_PDI$`a)`$`Crown radius (m)` <- ranges$max[ranges$Descriptor == 'Crown radius (m)']
  spectrumTrees_PDI$`b)`$`Crown radius (m)` <- ranges$center[ranges$Descriptor == 'Crown radius (m)']
  spectrumTrees_PDI$`c)`$`Crown radius (m)` <- ranges$min[ranges$Descriptor == 'Crown radius (m)']
  
  spectrumTrees_PDI$`a)`$`Total height (m)` <- ranges$max[ranges$Descriptor == 'Total height (m)']
  spectrumTrees_PDI$`b)`$`Total height (m)` <- ranges$center[ranges$Descriptor == 'Total height (m)']
  spectrumTrees_PDI$`c)`$`Total height (m)` <- ranges$min[ranges$Descriptor == 'Total height (m)']
  
  spectrumTrees_PDI$`a)`$`Lower crown height (m)` <- ranges$max[ranges$Descriptor == 'Lower crown height (m)']
  spectrumTrees_PDI$`b)`$`Lower crown height (m)` <- ranges$center[ranges$Descriptor == 'Lower crown height (m)']
  spectrumTrees_PDI$`c)`$`Lower crown height (m)` <- ranges$min[ranges$Descriptor == 'Lower crown height (m)']
  
  spectrumTrees_PDI$`a)`$`Diameter at breast height (cm)` <- ranges$max[ranges$Descriptor == 'Diameter at breast height (cm)']
  spectrumTrees_PDI$`b)`$`Diameter at breast height (cm)` <- ranges$center[ranges$Descriptor == 'Diameter at breast height (cm)']
  spectrumTrees_PDI$`c)`$`Diameter at breast height (cm)` <- ranges$min[ranges$Descriptor == 'Diameter at breast height (cm)']
  
  plotRanges_PDI <- spectrumTrees_PDI %>%
    names() %>%
    map(~{
      type <- .
      list(
        `Crown transparency (%)` = seq(ranges$min[ranges$Descriptor == 'Crown transparency (%)'],ranges$max[ranges$Descriptor == 'Crown transparency (%)'],length.out = 100),
        `Missing crown (%)` = seq(ranges$min[ranges$Descriptor == 'Missing crown (%)'],ranges$max[ranges$Descriptor == 'Missing crown (%)'],length.out = 100)) %>%
        expand.grid() %>%
        as_tibble() %>%
        mutate(ID = 1) %>%
        left_join(spectrumTrees_PDI[[type]] %>%
                    select(-`Missing crown (%)`,-`Crown transparency (%)`) %>%
                    mutate(ID = 1),by = 'ID') %>%
        select(-ID) %>%
        mutate(`Crown condition (%)` = crownCondition(`Missing crown (%)`,`Crown transparency (%)`),
               `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,`Total height (m)`,`Lower crown height (m)`,`Crown condition (%)`)) %>%
        mutate(PDI = predict(PDIrf,newdata = .))
    }) %>%
    set_names(names(spectrumTrees_PDI))
  
  PDI_response_plot <- plotRanges_PDI %>%
    names() %>%
    map(~{
      type <- .
      pl <- ggplot(plotRanges_PDI[[type]],aes(x = `Missing crown (%)`,y = `Crown transparency (%)`,fill = PDI,z = PDI)) +
        geom_raster() +
        geom_contour(colour = 'black',binwidth = 0.1) +
        geom_text_contour(stroke = 0.2,binwidth = 0.1,min.size = 10,size = 3) +
        scale_fill_gradient2(low = 'green',mid = 'orange',high = 'red',midpoint = 0.5,limits = c(0,1)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_bw(base_size = 10) +
        theme(plot.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold'),
              legend.title = element_text(face = 'bold')) +
        coord_fixed() +
        labs(title = type,
             caption = str_c('Crown radius (m) = ',plotRanges_PDI[[type]]$`Crown radius (m)`[1] %>% round(3),'\n',
                             'Total height (m) = ',plotRanges_PDI[[type]]$`Total height (m)`[1] %>% round(3),'\n',
                             'Lower crown height (m) = ',plotRanges_PDI[[type]]$`Lower crown height (m)`[1] %>% round(3),'\n',
                             'Diameter at breast height (cm) = ',plotRanges_PDI[[type]]$`Diameter at breast height (cm)`[1] %>% round(3)
             )
        )
      return(pl)
    }) %>%
    set_names(names(spectrumTrees_PDI))
  
  PDI_legend <- get_legend(PDI_response_plot$`a)`)
  
  PDI_response_plot <- PDI_response_plot %>%
    map(~{
      . + guides(fill = FALSE)
    })
  
  PDI_response_plot <- c(PDI_response_plot,list(legend = PDI_legend))
  
  return(PDI_response_plot)
}