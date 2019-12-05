
optimalParams <- function(tuning_results){
  tuning_results %>%
    filter(MAE == min(MAE)) %>%
    {list(mtry = .$mtry[1],ntree = .$ntree[1])}
}