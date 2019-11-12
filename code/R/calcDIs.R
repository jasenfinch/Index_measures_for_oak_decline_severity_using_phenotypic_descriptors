#' @importFrom purrr map_chr
#' @importFrom readr read_csv
#' @export

calcDIs <- function(seed = 1234){
  files <- list.files(system.file('phenotypeDataCollectionSheets',package = 'pdi'),full.names = T)

  phenoData <- files %>%
    map(readPhenotypeSheet)

  locations <- phenoData %>%
    map_chr(~{.$Location})

  names(phenoData) <- locations

  phenoData <- dataCorrections(phenoData)

  phenoData <- phenoData %>%
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
  analysisTable <- makeAnalysisTable(phenoData)

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

  suppressMessages(correctStatuses <- read_csv(system.file('other/nathan_corrected_labels.csv',package = 'pdi')))

  correctedPhenoData <- correctedPhenoData %>%
    select(-Status) %>%
    left_join(correctStatuses %>%
                select(Location,ID,SimpleSymptoms,ChosenGroup), by = c("Location", "ID")) %>%
    rename(Status = SimpleSymptoms)

  correctedAnalysisTable <- makeAnalysisTable(correctedPhenoData)

  set.seed(seed)

  unsupervisedRF <- rf(correctedAnalysisTable,NULL,100)

  DIs <- unsupervisedRF %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = minMaxScale(PDI),
           DAI = (2 * minMaxScale(DAI) - 1) * -1) %>%
    # mutate(DAI = 1 - DAI,PDI = 1 - PDI) %>%
    bind_cols(correctedPhenoData %>%
                select(Location,`Tree No`,Status,ChosenGroup))
  return(list(DIs = DIs,unsupervisedRF = unsupervisedRF,data = analysisTable))
}
