
descriptorAdjustmentPlot <- function(phenoData,correctedPhenoData){
  dbh <- varSummary(phenoData)
  
  corrected_dbh <- varSummary(correctedPhenoData)
  
  overallMean <- mean(phenoData$`Diameter at breast height (cm)`)
  
  ylims <- c(
    dbh %>% 
      filter(Mean == min(Mean)) %>%
      {.$Mean[1] - .$SE[1] - 1},
    dbh %>% 
      filter(Mean == max(Mean)) %>%
      {.$Mean[1] + .$SE[1] + 1}
  )
  
  descriptor_adjustment <- list(
    a = ggplot(dbh,aes(x = Location,y = Mean,)) +
      geom_hline(yintercept = overallMean,linetype = 2,colour = 'red') +
      geom_errorbar(aes(ymin = Mean - SE,ymax = Mean + SE),width = 0.5) +
      geom_point(colour = 'steelblue') +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = 'bold'),
            axis.text.x = element_text(angle = 20,hjust = 1),
            axis.title = element_text(face = 'bold')) +
      labs(title = 'Unadjusted',
           y = 'Diameter at\nbreast height (cm)',
           x = '') +
      ylim(ylims[1],ylims[2]),
    b = ggplot(corrected_dbh,aes(x = Location,y = Mean,)) +
      geom_hline(yintercept = overallMean,linetype = 2,colour = 'red') +
      geom_errorbar(aes(ymin = Mean - SE,ymax = Mean + SE),width = 0.5) +
      geom_point(colour = 'steelblue') +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = 'bold'),
            axis.text.x = element_text(angle = 20,hjust = 1),
            axis.title = element_text(face = 'bold')) +
      labs(title = 'Adjusted',
           y = 'Adjusted diameter at\nbreast height (cm)',
           x = '') +
      ylim(ylims[1],ylims[2])
  )
  
  return(descriptor_adjustment)
}

