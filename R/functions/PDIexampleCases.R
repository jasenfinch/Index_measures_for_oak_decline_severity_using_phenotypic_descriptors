
PDIexampleCases <- function(site_corrected_analysis_suitable_data,decline_indexes){
  PDI_quantiles <- decline_indexes$PDI %>%
    quantile() %>%
    .[c("0%","50%",'100%')]
  # .[c("25%","50%",'75%')]
  
  PDI_values <- PDI_quantiles %>%
    map_dbl(~{
      a <- decline_indexes$PDI %>%
        {. - .x} %>%
        abs()
      decline_indexes$PDI[which(a == min(a))[1]]
    })
  
  spectrumTrees_PDI <- list(
    `a) Healthy` = site_corrected_analysis_suitable_data[decline_indexes$PDI == PDI_values[['0%']],],
    `b) Moderate decline` = site_corrected_analysis_suitable_data[decline_indexes$PDI == PDI_values[['50%']],],
    `c) Severe decline` = site_corrected_analysis_suitable_data[decline_indexes$PDI == PDI_values[['100%']],]
  )
  
  return(spectrumTrees_PDI)
}