
tuneModel <- function(site_corrected_analysis_suitable_data,index,index_size){
  mtry <- seq(2,26,by = 4)
  ntree <- seq(1,4,by = 1) %>%
    {10 ^ .}

  comb <- expand.grid(mtry,ntree) %>%
    as_tibble() %>%
    set_names(c('mtry','ntree'))
  
  res <- comb %>%
    split(1:nrow(.)) %>%
    map(~{
      n <- .
      set.seed(1234)
        randomForest(site_corrected_analysis_suitable_data,y = index,ntree = n$ntree,mtry = n$mtry) %>%
          .$predicted %>%
          {abs(. - index)} %>%
          mean(na.rm = TRUE) %>%
          tibble(mtry = n$mtry,ntree = n$ntree,MAE = .,PMAE = . / index_size * 100)
    }) %>%
    bind_rows()
  
  return(res)
}
