
descriptorImportance <- function(rf_model){
  rf_model %>%
    {
      imp <- importance(.)
      feat <- rownames(imp)
      tibble(Feature = feat,`%IncMSE` = imp[,'%IncMSE'],
             IncNodePurity = imp[,'IncNodePurity'])
    } %>%
    arrange(desc(`%IncMSE`))
}