#' dataCorrections
#' @description a specific function to correct phenotype data inaccuracies
#' @param phenoData tibble containing phenotype data

dataCorrections <- function(phenoData){
  phenoData$`Big Wood`$Description$Value[
    phenoData$`Big Wood`$Description$Value[phenoData$`Big Wood`$Description$Descriptor == 'Tree No'] %>% str_detect('CONTROL') &
      phenoData$`Big Wood`$Description$Descriptor == 'Symptomatic'
    ] <- 'N'
  
  phenoData$`Big Wood`$Description$Value[
    !(phenoData$`Big Wood`$Description$Value[phenoData$`Big Wood`$Description$Descriptor == 'Tree No'] %>% str_detect('CONTROL')) &
      phenoData$`Big Wood`$Description$Descriptor == 'Symptomatic'
    ] <- 'Y'
  
  phenoData <- phenoData %>%
    map(preparePhenotypeData) %>%
    bind_rows(.id = 'Location')
  
  phenoData$`Crown transparency (%)`[phenoData$Location %in% c('Attingham','Langdale')] <- 100 - phenoData$`Crown transparency (%)`[phenoData$Location %in% c('Attingham','Langdale')]
  
  phenoData <- phenoData %>%
    correctStatuses()
  
  phenoData <- phenoData %>%
    mutate_at(vars(contains(coll('(cm)'))),~{. * 10}) %>%
    rename_at(vars(contains(coll('(cm)'))),~{str_replace(.,'(cm)','mm')})
  
  return(phenoData)
}