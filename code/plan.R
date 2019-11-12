# Workflow plan

plan <- drake_plan(
  
  ## Locate data files
  pheno_files = list.files('data/phenotype_collection_sheets',full.names = T),
  
  ## read in data, correct data sheet mistakes and calculate additional desecriptors
  pheno_data = pheno_files %>%
    map(readPhenotypeSheet) %>%
    {set_names(.,map_chr(.,~{.$Location}))} %>%
    dataCorrections() %>%
    calcAdditionalDescriptors()
  
  ## make analysis suitable table
  ## apply site corrections
  ## make site corrected analysis suitable table
  ## run unsupervised random forest analysis
  ## calculate decline indexes
)