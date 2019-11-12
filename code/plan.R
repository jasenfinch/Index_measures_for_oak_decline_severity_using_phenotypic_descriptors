# Workflow plan

plan <- drake_plan(
  
  ## Locate data files
  pheno_files = 'data/phenotype_collection_sheets' %>%
    list.files(full.names = T),
  
  ## read in data, correct data sheet mistakes
  pheno_data = pheno_files %>%
    map(readPhenotypeSheet) %>%
    {set_names(.,map_chr(.,~{.$Location}))} %>%
    dataCorrections(), 
  
  ## calculate additional desecriptors
  
  pheno_data_with_additional_descriptors = pheno_data %>%
    calcAdditionalDescriptors(),
  
  ## make analysis suitable table
  analysis_suitable_data = pheno_data_with_additional_descriptors %>%
    makeAnalysisTable(),
  
  ## collect phenotypic descriptor information into table
  
  phenotypic_descriptor_info = analysis_suitable_data %>%
    descriptorInfo(),
    
  ## apply site corrections and recalculate additional descriptors
  site_corrected_pheno_data = pheno_data %>%
    siteCorrection() %>%
    calcAdditionalDescriptors(),
  
  ## make site corrected analysis suitable table
  site_corrected_analysis_suitable_data = site_corrected_pheno_data %>%
    makeAnalysisTable(),
  
  ## run unsupervised random forest analysis
  unsupervised_rf = site_corrected_analysis_suitable_data %>%
    rf(cls = NULL,nreps = 100),
  
  ## calculate decline indexes
  DIs = unsupervised_rf %>%
    calcDIs(site_corrected_pheno_data),
  
  ## render manuscript
  manuscript = render(knitr_in('manuscript/manuscript.Rmd'),quiet = T)
)