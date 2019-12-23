
DAIlimeAnalysis <- function(site_corrected_analysis_suitable_data,DAI_rf_model,decline_indexes){
  DAI_explainer <- lime(site_corrected_analysis_suitable_data, DAI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)
  
  dat <- site_corrected_analysis_suitable_data %>%
    bind_cols(decline_indexes %>%
                select(PDI,DAI)) %>%
    filter(PDI > 0.5)
  
  spectrumTrees_DAI <- list(
    `neutral` = dat %>%
      filter(abs(DAI) == min(abs(DAI))),
    `moderateAOD` = dat %>%
      filter(abs(DAI - 0.5) == min(abs(DAI - 0.5))),
    `severeAOD` = dat %>%
      filter(DAI == max(DAI)),
    `moderateCOD` = dat %>%
      filter(abs(DAI + 0.5) == min(abs(DAI + 0.5))),
    `severeCOD` = dat %>%
      filter(DAI == min(DAI))
  ) %>%
    map(select,-PDI,-DAI) %>%
    bind_rows()
  
  DAI_explanation <- explain(spectrumTrees_DAI, DAI_explainer, n_features = 4)
  
  return(DAI_explanation)
}