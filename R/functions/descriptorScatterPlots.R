
descriptorScatterPlots <- function(site_corrected_analysis_suitable_data,decline_indexes){
  list(
    PDI_numeric = site_corrected_analysis_suitable_data %>%
      rowid_to_column(var = 'ID') %>%
      bind_cols(select(decline_indexes,PDI)) %>%
      select_if(is.numeric) %>%
      gather(Descriptor,Value,-ID,-PDI) %>%
      ggplot(aes(x = Value,y = PDI)) +
      geom_point(shape = 21,fill = ptol_pal()(1)) +
      theme_bw(base_size = 10) +
      facet_wrap(~Descriptor,scales = 'free',ncol = 3) +
      labs(title = 'PDI against continuous descriptors',
           x = 'Descriptor value') +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold')),
    
    PDI_factor =  site_corrected_analysis_suitable_data %>%
      select_if(is.factor) %>%
      rowid_to_column(var = 'ID') %>%
      bind_cols(select(decline_indexes,PDI)) %>%
      gather(Descriptor,Value,-ID,-PDI) %>%
      ggplot(aes(x = Value,y = PDI)) +
      geom_point(shape = 21,fill = ptol_pal()(1)) +
      theme_bw() +
      facet_wrap(~Descriptor,scales = 'free',ncol = 3) +
      labs(title = 'PDI versus categorical descriptors',
           x = 'Descriptor value') +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold')),
    
    DAI_numeric = site_corrected_analysis_suitable_data %>%
      rowid_to_column(var = 'ID') %>%
      bind_cols(select(decline_indexes,DAI)) %>%
      select_if(is.numeric) %>%
      gather(Descriptor,Value,-ID,-DAI) %>%
      ggplot(aes(x = Value,y = DAI)) +
      geom_point(shape = 21,fill = ptol_pal()(1)) +
      theme_bw() +
      facet_wrap(~Descriptor,scales = 'free',ncol = 3) +
      labs(title = 'DAI versus continuous descriptors',
           x = 'Descriptor value') +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold')),
    
    DAI_factor = site_corrected_analysis_suitable_data %>%
      select_if(is.factor) %>%
      rowid_to_column(var = 'ID') %>%
      bind_cols(select(decline_indexes,DAI)) %>%
      gather(Descriptor,Value,-ID,-DAI) %>%
      ggplot(aes(x = Value,y = DAI)) +
      geom_point(shape = 21,fill = ptol_pal()(1)) +
      theme_bw() +
      facet_wrap(~Descriptor,scales = 'free',ncol = 3) +
      labs(title = 'DAI versus categorical descriptors',
           x = 'Descriptor value') +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold'))
  )
}
