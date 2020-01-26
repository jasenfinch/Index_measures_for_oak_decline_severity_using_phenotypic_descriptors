
DAIlimeAnalysis <- function(site_corrected_analysis_suitable_data,DAI_rf_model,DAI_example_cases){
  DAI_explainer <- lime(site_corrected_analysis_suitable_data, DAI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)
  
  DAI_explanation <- explain(DAI_example_cases[-4] %>%
                               bind_rows(),
                             DAI_explainer, n_features = 4)
  
  return(DAI_explanation)
}