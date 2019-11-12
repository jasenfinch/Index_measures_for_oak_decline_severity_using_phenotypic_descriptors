#' calcAdditionalDescriptors
#' @description calculate additional descriptors
#' @param phenoData tibble containing phenotype data

calcAdditionalDescriptors <- function(phenoData){
  phenoData %>%
    mutate(`Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                                  `Crown transparency (%)`),
           `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                              `Total height (m)`,
                                              `Lower crown height (m)`,
                                              `Crown condition (%)`),
           `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (cm)`,
                                                    `Active bleeds`,
                                                    `Black staining size (cm)`,
                                                    `Black staining`,
                                                    `Diameter at breast height (cm)`),
           `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (cm)`)
    )
}