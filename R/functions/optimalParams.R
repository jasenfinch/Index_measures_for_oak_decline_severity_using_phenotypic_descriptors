
optimalParams <- function(tuning_results){
  tuning_results %>%
    filter(PMAE == min(PMAE)) %>%
    {list(mtry = .$mtry[1],ntree = .$ntree[1],PMAE = .$PMAE[1])}
}