
calcDIs <- function(rfModels,phenoData){
  rfModels %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = minMaxScale(PDI),
           DAI = (2 * minMaxScale(DAI) - 1)) %>%
    bind_cols(phenoData %>%
                select(Location,`Tree No`,Status,ChosenGroup))
}