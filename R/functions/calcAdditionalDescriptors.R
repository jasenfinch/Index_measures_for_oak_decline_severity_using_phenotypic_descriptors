#' calcAdditionalDescriptors
#' @description calculate additional descriptors
#' @param phenoData tibble containing phenotype data

calcAdditionalDescriptors <- function(phenoData){
  phenoData %>%
    mutate(`Live crown ratio (%)` = pdi::liveCrownRatio(`Total height (m)`,
                                                   `Lower crown height (m)`),
           `Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                                  `Crown transparency (%)`),
           `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                              `Total height (m)`,
                                              `Lower crown height (m)`,
                                              `Crown condition (%)`),
           `Bleed prevalence (%)` = bleedPrevalence(`Active bleed length (mm)`,
                                                    `Active bleeds`,
                                                    `Black staining length (mm)`,
                                                    `Black staining`,
                                                    `Diameter at breast height (m)`),
           `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (m)`)
    )
}