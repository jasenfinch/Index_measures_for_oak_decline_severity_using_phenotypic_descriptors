#' siteCorrectionFactors
#' @description return site correction correction factors of selected descriptors
#' @param phenoData phenoData tibble containing phenotype data
#' @param descriptors columns of phenoData on which calculate site correction factors
#' @export


siteCorrectionFactors <- function(phenoData,descriptors = c("Diameter at breast height (mm)",
                                                            "Lower crown height (m)",
                                                            "Timber height (m)",
                                                            "Total height (m)",
                                                            "Crown radius (m)")){

  siteCorrect <- phenoData %>%
    select(Location,ID,descriptors) %>%
    gather('Descriptor','Value',-Location,-ID)

  overallMeans <- siteCorrect %>%
    group_by(Descriptor) %>%
    summarise(Mean = mean(Value))

  siteCorrections <- siteCorrect %>%
    group_by(Location,Descriptor) %>%
    summarise(Mean = mean(Value)) %>%
    tbl_df() %>%
    split(.$Descriptor) %>%
    map(~{
      d <- .
      d %>%
      mutate(Correction = Mean - ({overallMeans %>% filter(Descriptor == d$Descriptor[1]) %>% .$Mean}))
    }) %>%
    bind_rows() %>%
    select(Descriptor,Location,Mean,Correction)
  return(siteCorrections)
}
