#' siteDifferencesRFplot
#' @description create MDS scatter and random forest variable importance plots for site phenotypic differences
#' @param site_differences_rf list of site supervised random forest models
#' @param phenoData tibble containing phenotype data

siteDifferencesRFplot <- function(site_differences_rf,phenoData){
  locationMDS <- site_differences_rf %>%
    mds() %>%
    bind_cols(phenoData %>%
                select(Location))
  
  locationMargin <- site_differences_rf %>%
    margins() %>%
    summarise(Margin = mean(Margin)) %>%
    mutate(x = 0.4,y = 0.5,label = str_c('Margin = ',Margin %>% round(2)))
  
  locationImportance <- site_differences_rf %>%
    map(~{
      imp <- importance(.)
      desc <- rownames(imp)
      imp <- imp %>%
        as_tibble() %>%
        mutate(Descriptor = desc) %>%
        select(Descriptor,MeanDecreaseAccuracy,MeanDecreaseGini)
    }) %>%
    bind_rows(.id = 'Rep') %>%
    group_by(Descriptor) %>%
    summarise(MeanDecreaseAccuracy = mean(MeanDecreaseAccuracy),
              GiniImportance = mean(MeanDecreaseGini)) %>%
    arrange(MeanDecreaseAccuracy) %>%
    mutate(Descriptor = factor(Descriptor,levels = Descriptor))
  
  descriptorLabels <- locationImportance$Descriptor %>%
    as.character() %>%
    {
      .[str_detect(.,coll('Agrilus exit hole density (m^-2)'))] <- expression(Agrillus~exit~hole~density ( m^-2 ) )
      .[str_detect(.,coll('Crown volume (m^3)'))] <- expression(Crown~volume ( m^3 ) )
      return(.)
    }
  
  site_differences <- list(
    a = ggplot() +
      geom_point(data = locationMDS,
                 aes(x = `Dimension 1`, 
                     y = `Dimension 2`,
                     fill = Location),
                 shape = 21,
                 size = 3) +
      scale_fill_ptol() +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold'),
            legend.position = 'bottom') +
      coord_fixed() +
      guides(fill = guide_legend(ncol = 2,title.position = "top")) +
      labs(title = 'a) MDS scatter plot',
           fill = 'Site'),
    b = ggplot(locationImportance,aes(x = `MeanDecreaseAccuracy`,y = Descriptor)) +
      geom_point(shape = 21,fill = ptol_pal()(1),size = 3) +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold',size = 10)) +
      labs(title = 'b) Descriptor\nimportance',
           y = NULL,
           x = 'Mean decrease in\naccuracy') +
      scale_y_discrete(labels = descriptorLabels)
  )
  
  return(site_differences)
}
