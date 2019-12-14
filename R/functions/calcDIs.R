
calcDIs <- function(rfModels,phenoData){
  rfModels %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = PDI %>%
             {. * -1} %>%
             minMaxScale(),
           DAI = DAI %>%
             minMaxScale() %>%
             {2 * . - 1}) %>%
    bind_cols(phenoData %>%
                select(Location,`Tree No`,Status,ChosenGroup))
}