
PDIlimeAnalysis <- function(site_corrected_analysis_suitable_data,PDI_rf_model,decline_indexes){
  PDI_explainer <- lime(site_corrected_analysis_suitable_data, PDI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)
  
  spectrumTrees_PDI <- list(
    `a)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == min(decline_indexes$PDI)),],
    `b)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$PDI - 0.5) == min(abs(decline_indexes$PDI - 0.5))),],
    `c)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == max(decline_indexes$PDI)),]
  ) %>%
    bind_rows()
  
  PDI_explanation <- explain(spectrumTrees_PDI, PDI_explainer, n_features = 4)
  
  return(PDI_explanation)
}