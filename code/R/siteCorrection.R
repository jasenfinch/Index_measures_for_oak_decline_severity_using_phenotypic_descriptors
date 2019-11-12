#' siteCorrection
#' @description perform a site correction of selected descriptors
#' @param phenoData phenoData tibble containing phenotype data
#' @param descriptors columns of phenoData on which to perform site correction
#' @export

siteCorrection <- function(phenoData,descriptors = c("Diameter at breast height (cm)",
                                                     "Lower crown height (m)",
                                                     "Timber height (m)",
                                                     "Total height (m)",
                                                     "Crown radius (m)")){
  siteCorrect <- phenoData %>%
    select(Location,ID,descriptors) %>%
    gather('Descriptor','Value',-Location,-ID)

  siteCorrections <- siteCorrectionFactors(phenoData,descriptors)

  siteCorrect <- siteCorrect %>%
    split(.$Descriptor) %>%
    map(~{
      d <- .
      d %>%
        split(.$Location) %>%
        map(~{
          d <- .
          d %>%
            mutate(Value = Value - { siteCorrections %>%
                filter(Descriptor == d$Descriptor[1],Location == d$Location[1]) %>% .$Correction})
        }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    spread(Descriptor,Value)

  correctedPhenoData <- phenoData %>%
    select(-descriptors) %>%
    left_join(siteCorrect,by = c("Location", "ID"))

  return(correctedPhenoData)
}
