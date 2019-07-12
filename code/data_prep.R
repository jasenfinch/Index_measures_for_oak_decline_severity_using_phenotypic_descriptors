phenoData <- {
  files <- list.files(system.file('phenotypeDataCollectionSheets',package = 'phi'),full.names = T)
  
  phenoData <- files %>%
    map(readPhenotypeSheet) 
  
  locations <- phenoData %>%
    map_chr(~{.$Location})
  
  names(phenoData) <- locations
  
  phenoData <- dataCorrections(phenoData)
  
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

DIs <- calcDIs()

correctedPhenoData <- siteCorrection(phenoData) %>%
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
