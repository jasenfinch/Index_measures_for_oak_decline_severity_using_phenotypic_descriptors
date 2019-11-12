#' correctStatuses
#' @description 
#' @param phenoData tibble containing phenotype data

correctStatuses <- function(phenoData){
  suppressMessages(correctStatuses <- read_csv('data/other/nathan_corrected_labels.csv'))
  
  phenoData %>%
    select(-Status) %>%
    left_join(correctStatuses %>%
                select(Location,ID,SimpleSymptoms,ChosenGroup), by = c("Location", "ID")) %>%
    rename(Status = SimpleSymptoms)
}