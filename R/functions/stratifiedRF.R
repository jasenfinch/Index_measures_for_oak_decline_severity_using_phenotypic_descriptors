
stratifiedRF <- function(d,cl,n){
  ss <- cl %>%
    table() %>%
    min() %>%
    rep(cl %>% unique() %>% length())
  
  1:n %>%
    map(~{
      randomForest(d,y = cl,strata = cl,sampsize = ss,proximity = TRUE,importance = TRUE)
    })
}