
DAIexampleCases <- function(site_corrected_analysis_suitable_data,decline_indexes){
  dat <- site_corrected_analysis_suitable_data %>%
    bind_cols(decline_indexes %>%
                select(PDI,DAI)) %>%
    filter(PDI > 0.5)
  
  spectrumTrees_DAI <- list(
    `neutralAOD` = dat %>%
      filter(abs(DAI) == min(abs(DAI))),
    `moderateAOD` = dat %>%
      filter(abs(DAI - 0.5) == min(abs(DAI - 0.5))),
    `severeAOD` = dat %>%
      filter(DAI == max(DAI)),
    `neutralCOD` = dat %>%
      filter(abs(DAI) == min(abs(DAI))),
    `moderateCOD` = dat %>%
      filter(abs(DAI + 0.5) == min(abs(DAI + 0.5))),
    `severeCOD` = dat %>%
      filter(DAI == min(DAI))
  ) %>%
    map(select,-PDI,-DAI)
  
  return(spectrumTrees_DAI)
}