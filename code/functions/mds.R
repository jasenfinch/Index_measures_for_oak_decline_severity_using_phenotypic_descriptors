#' mds
#' @description perform multidimensional scaling of random forest proximities
#' @param rfModels list containing random forest models

mds <- function(rfModels){
  rfModels %>%
    map(~{.$proximity %>%
        as_tibble() %>%
        rowid_to_column(var = 'Sample1') %>%
        gather('Sample2','Proximity',-Sample1)}) %>%
    bind_rows(.id = 'Iteration') %>%
    mutate(Sample2 = as.numeric(Sample2)) %>%
    group_by(Sample1,Sample2) %>%
    summarise(Proximity = mean(Proximity)) %>%
    spread(Sample2,Proximity) %>%
    tbl_df() %>%
    select(-Sample1) %>%
    as.matrix() %>%
    {1 - .} %>%
    cmdscale() %>%
    as_tibble() %>%
    rename(`Dimension 1` = V1,`Dimension 2` = V2)
}
