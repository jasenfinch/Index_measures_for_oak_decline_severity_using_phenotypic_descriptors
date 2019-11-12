# Workflow plan

plan <- drake_plan(
  
  ## Locate data files
  phenotype_data_file_paths = 'data/phenotype_collection_sheets' %>%
    list.files(full.names = T),
  
  ## read in data, correct data sheet mistakes
  pheno_data = phenotype_data_file_paths %>%
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
  
  ## create site location map
  
  site_location_map = locationMap(),
  
  ## Analyse site differences using supervised random forest
  
  site_differences_rf = analysis_suitable_data %>%
    rf(cls = pheno_data_with_additional_descriptors$Location %>% factor(),
       nreps = 100),
  
  ## site differences MDS and importance plots
  
  site_differences_mds_plot = site_differences_rf %>%
    siteDifferencesRFplot(pheno_data_with_additional_descriptors),
    
  ## calculate descriptor correction factors
  
  site_correction_factors <- pheno_data_with_additional_descriptors %>%
    siteCorrectionFactors(),
  
  ## apply site corrections and recalculate additional descriptors
  site_corrected_pheno_data = pheno_data %>%
    siteCorrection() %>%
    calcAdditionalDescriptors(),
  
  ## plot descriptor adjustment example using dbh
  
  descriptor_adjustment_example = pheno_data_with_additional_descriptors %>%
    descriptorAdjustmentPlot(site_corrected_pheno_data),
  
  ## make site corrected analysis suitable table
  site_corrected_analysis_suitable_data = site_corrected_pheno_data %>%
    makeAnalysisTable(),
  
  ## run unsupervised random forest analysis
  unsupervised_rf = site_corrected_analysis_suitable_data %>%
    rf(cls = NULL,nreps = 100),
  
  ## calculate decline indexes
  decline_indexes = unsupervised_rf %>%
    calcDIs(site_corrected_pheno_data),
  
  ## generate PDI predictive random forest model
  PDI_rf_model = site_corrected_analysis_suitable_data %>%
    {set.seed(1234)
      randomForest(.,y = decline_indexes$PDI,
                 ntree = 10000)},
  
  ## generate DAI predictive random forest model
  DAI_rf_model = site_corrected_analysis_suitable_data %>%
    {set.seed(1234)
      randomForest(.,y = decline_indexes$DAI,
                   ntree = 10000,
                   mtry = 11)},
  
  ## create descriptor contribution plots
  descriptor_contribution_plots = descriptorImportancePlots(PDI_rf_model,DAI_rf_model),
  
  ## PDI rf model response surfaces
  PDI_response_surfaces = PDIresponseSurfaces(PDI_rf_model,
                                              decline_indexes,
                                              site_corrected_analysis_suitable_data),
  
  ## DAI rf model response surfaces
  DAI_response_surfaces = DAIresponseSurfaces(DAI_rf_model,
                                              decline_indexes,
                                              site_corrected_analysis_suitable_data),
  
  ## render manuscript
  manuscript = render(knitr_in('manuscript/manuscript.Rmd'),quiet = T),
  
  ## render supplementary
  supplementary = render(knitr_in('manuscript/supplementary.Rmd'),quiet = T)
)