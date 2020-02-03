
PDIlimeAnalysis <- function(site_corrected_analysis_suitable_data,PDI_rf_model,PDI_example_cases){
  PDI_explainer <- lime(site_corrected_analysis_suitable_data, PDI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)
  
  PDI_explanation <- explain(PDI_example_cases %>%
                               bind_rows(),
                             PDI_explainer, n_features = 4)
  
  return(PDI_explanation)
}