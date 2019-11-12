#' calcPHI
#' @description calculate phenotypic health indexes
#' @param rfModels list containing random forest models
#' @param phenoData tibble containing phenotype data
#' @importFrom tibble tibble
#' @export

calcPHI <- function(rfModels,phenoData){

  prox <- rfModels %>%
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
    as.matrix()

  {1 - prox} %>%
    cmdscale(k = 1) %>%
    {tibble(PHI = .[,1])} %>%
    bind_cols(select(phenoData,Location,`Tree No`,Status,ChosenGroup)) %>%
    mutate(PHI = (PHI - min(PHI)) / (max(PHI) - min(PHI)))
}
