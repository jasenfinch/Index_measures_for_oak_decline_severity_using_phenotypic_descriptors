
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
  
  IDs <- decline_indexes %>%
    filter(PDI %in% PDI_values) %>%
    arrange(PDI) %>%
    .$ID
  
  spectrumTrees_PDI <- list(
    site_corrected_analysis_suitable_data[decline_indexes$PDI == PDI_values[['0%']],],
    site_corrected_analysis_suitable_data[decline_indexes$PDI == PDI_values[['50%']],],
    site_corrected_analysis_suitable_data[decline_indexes$PDI == PDI_values[['100%']],]
  ) %>%
    set_names(str_c(c('Healthy','Moderate decline','Severe decline'),'-',IDs))
  
  return(spectrumTrees_PDI)
}