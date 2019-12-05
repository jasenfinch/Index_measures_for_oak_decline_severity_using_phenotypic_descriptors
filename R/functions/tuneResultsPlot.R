
tuneResultsPlot <- function(PDI_rf_tune_results,DAI_rf_tune_results) {
  list(PDI = PDI_rf_tune_results,
       DAI = DAI_rf_tune_results) %>%
    bind_rows(.id = 'Index') %>%
    mutate(ntree = factor(ntree)) %>%
    ggplot(aes(x = mtry,y = MAE,colour = ntree)) +
    geom_line() +
    scale_colour_ptol() +
    theme_bw() +
    theme(axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold')) +
    labs(y = 'Mean absolute error (MAE)') +
    facet_wrap(~Index,scales = 'free')
}
