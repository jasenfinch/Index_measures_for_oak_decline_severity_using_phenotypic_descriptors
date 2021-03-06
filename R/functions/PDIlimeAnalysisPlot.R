
PDIlimeAnalysisPlot <- function(PDI_lime_analysis,PDI_example_cases,decline_indexes){
  
  IDs <- PDI_example_cases %>%
    names() %>%
    str_split_fixed('-',2) %>%
    .[,2] %>%
    as.numeric()
  
  name <- PDI_example_cases %>%
    names() %>%
    str_split_fixed('-',2) %>%
    .[,1]
  
  cases <- tibble(case = 1:3 %>% as.character(),
                  name = name,
                  PDI = decline_indexes %>%
                    filter(ID %in% IDs) %>%
                    arrange(PDI) %>%
                    .$PDI) %>%
    mutate(PDI = signif(PDI,3))
  
  PDI_lime_analysis %>%
    left_join(cases,by = 'case') %>%
    mutate(direction = sign(feature_weight) %>%
             factor(levels = c(-1,1))) %>%
    split(.$case) %>%
    map(~{
      d <- .
      d %>%
        mutate(feature_desc = factor(feature_desc,levels = d$feature_desc[order(abs(feature_weight))])) %>%
        ggplot(.,aes(x = feature_desc,y = feature_weight)) +
        geom_bar(aes(fill = direction),stat = 'identity',colour = 'black') +
        scale_fill_manual(values = c(`1` = ptol_pal()(2)[1],`-1` = ptol_pal()(2)[2])) +
        theme_bw() +
        coord_flip() +
        guides(fill = FALSE) +
        lims(y = c(min(PDI_lime_analysis$feature_weight),max(PDI_lime_analysis$feature_weight))) +
        labs(title = d$name[1],
             subtitle = str_c('PDI: ',d$PDI %>% round(3)),
             x = 'Feature range',
             y = 'Weight') +
        theme(plot.title = element_text(face = 'bold'),
              axis.title = element_text(face = 'bold'))
    }) %>%
    wrap_plots(ncol = 1)
}