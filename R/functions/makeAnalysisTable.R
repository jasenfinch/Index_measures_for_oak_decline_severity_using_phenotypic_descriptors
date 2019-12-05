#' makeAnalysisTable
#' @description prepare phenoData table ready for random forest analysis
#' @param phenoData tibble containing phenotype data

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
