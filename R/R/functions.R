#' dataCorrections
#' @description a specific function to correct phenotype data inaccuracies
#' @param phenoData tibble containing phenotype data
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export

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

  return(phenoData)
}

#' makeAnalysisTable
#' @description prepare phenoData table ready for random forest analysis
#' @param phenoData tibble containing phenotype data
#' @importFrom dplyr select
#' @importFrom purrr map_dfc
#' @export

makeAnalysisTable <- function(phenoData){
  analysisTable <- phenoData %>%
    select(-Location,-ID,-GPS,-Status,-`Tree No`)

  if ('ChosenGroup' %in% names(analysisTable)) {
    analysisTable <- analysisTable %>%
      select(-ChosenGroup)
  }

  analysisTable[,{map_dfc(analysisTable,is.character) %>% as.logical()}] <- analysisTable[, {map_dfc(analysisTable,is.character) %>% as.logical()}] %>%
    map_dfc(factor)

  return(analysisTable)
}
