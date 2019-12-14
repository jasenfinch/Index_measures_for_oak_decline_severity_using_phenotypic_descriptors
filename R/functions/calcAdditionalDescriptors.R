#' calcAdditionalDescriptors
#' @description calculate additional descriptors
#' @param phenoData tibble containing phenotype data

calcAdditionalDescriptors <- function(phenoData){
  phenoData %>%
    mutate(`Live crown ratio (%)` = liveCrownRatio(`Total height (m)`,
                                                   `Lower crown height (m)`),
           `Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                                  `Crown transparency (%)`),
           `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                              `Total height (m)`,
                                              `Lower crown height (m)`,
                                              `Crown condition (%)`),
           `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (mm)`,
                                                    `Active bleeds`,
                                                    `Black staining size (mm)`,
                                                    `Black staining`,
                                                    `Diameter at breast height (mm)`),
           `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (mm)`)
    )
}