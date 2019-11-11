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

analysisTable <- makeAnalysisTable(correctedPhenoData)

set.seed(1234)
PDIrf <- randomForest(analysisTable,DIs$DIs$PDI,ntree = 10000)
DAIrf <- randomForest(analysisTable,DIs$DIs$DAI,ntree = 10000,mtry = 11)
