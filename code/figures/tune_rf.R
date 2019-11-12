# tuneRF(DIs$data,DIs$DIs$DAI,ntreeTry = 500)
# 
# mtry <- seq(2,26,by = 4)
# ntree <- c(10,100,1000,10000,100000)
# 
# a <- ntree %>%
#   map(~{
#     n <- .
#     map(mtry,~{
#       m <- .
#       randomForest(DIs$data,y = DIs$DIs$DAI,ntree = n,mtry = m) %>%
#         .$predicted %>%
#         {abs(. - DIs$DIs$DAI)} %>%
#         mean() %>%
#         tibble(MAE = .)
#     }) %>%
#       set_names(mtry) %>%
#       bind_rows(.id = 'mtry')
#   }) %>%
#   set_names(ntree) %>%
#   bind_rows(.id = 'ntree')