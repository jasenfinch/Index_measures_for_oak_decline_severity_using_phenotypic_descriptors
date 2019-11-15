library(lime)

loadd(site_corrected_analysis_suitable_data)

loadd(PDI_rf_model)
loadd(DAI_rf_model)
loadd(decline_indexes)

predict_model.randomForest <- function(x, newdata, type, ...) {
  res <- predict(x, newdata = newdata, ...)
  switch(
    type,
    raw = data.frame(Response = res, stringsAsFactors = FALSE),
    prob = as.data.frame(res, check.names = FALSE)
  )
}

model_type.randomForest <- function(x, ...) x$type

PDI_explainer <- lime(site_corrected_analysis_suitable_data, PDI_rf_model, bin_continuous = TRUE, quantile_bins = FALSE)

spectrumTrees_PDI <- list(
  `a)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == min(decline_indexes$PDI)),],
  `b)` = site_corrected_analysis_suitable_data[which(abs(decline_indexes$PDI - 0.5) == min(abs(decline_indexes$PDI - 0.5))),],
  `c)` = site_corrected_analysis_suitable_data[which(decline_indexes$PDI == max(decline_indexes$PDI)),]
) %>%
  bind_rows()

PDI_explanation <- explain(spectrumTrees_PDI, PDI_explainer, n_features = 4)

plot_features(PDI_explanation,ncol = 3)



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

plot_features(DAI_explanation,ncol = 3)
