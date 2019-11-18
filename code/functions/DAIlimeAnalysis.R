
DAIlimeAnalysis <- function(site_corrected_analysis_suitable_data,DAI_rf_model,decline_indexes){
  DAI_explainer <- lime(site_corrected_analysis_suitable_data, DAI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)
  
  DAI_explainer <- lime(site_corrected_analysis_suitable_data, DAI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)
  
  spectrumTrees_DAI <- list(
    `a)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI) == min(abs(decline_indexes$DAI))),],
    `b)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI - 0.5) == min(abs(decline_indexes$DAI - 0.5))),],
    `c)` = site_corrected_analysis_suitable_data[which(decline_indexes$DAI == max(decline_indexes$DAI)),],
    `d)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI) == min(abs(decline_indexes$DAI))),],
    `e)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$DAI + 0.5) == min(abs(decline_indexes$DAI + 0.5))),],
    `f)` = site_corrected_analysis_suitable_data[which(decline_indexes$DAI == min(decline_indexes$DAI)),]
  ) %>%
    bind_rows()
  
  DAI_explanation <- explain(spectrumTrees_DAI, DAI_explainer, n_features = 4)
  
  return(DAI_explanation)
}