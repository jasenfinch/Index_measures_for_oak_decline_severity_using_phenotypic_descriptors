#' dataCorrections
#' @description a specific function to correct phenotype data inaccuracies
#' @param phenoData tibble containing phenotype data

dataCorrections <- function(phenoData){
  phenoData$`Site 2`$Description$Value[
    phenoData$`Site 2`$Description$Value[phenoData$`Site 2`$Description$Descriptor == 'Tree No'] %>% str_detect('CONTROL') &
      phenoData$`Site 2`$Description$Descriptor == 'Symptomatic'
  ] <- 'N'
  
  phenoData$`Site 2`$Description$Value[
    !(phenoData$`Site 2`$Description$Value[phenoData$`Site 2`$Description$Descriptor == 'Tree No'] %>% str_detect('CONTROL')) &
      phenoData$`Site 2`$Description$Descriptor == 'Symptomatic'
  ] <- 'Y'
  
  phenoData <- phenoData %>%
    map(preparePhenotypeData) %>%
    bind_rows()
  
  phenoData$`Crown transparency (%)`[phenoData$Location %in% c('Site 1','Site 6')] <- 100 - phenoData$`Crown transparency (%)`[phenoData$Location %in% c('Site 1','Site 6')]
  
  phenoData <- phenoData %>%
    correctStatuses()
  
  return(phenoData)
}