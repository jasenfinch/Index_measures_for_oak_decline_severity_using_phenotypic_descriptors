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
  
  ## export phenotypic data
  export_pheno_data_with_additional_descriptors = pheno_data_with_additional_descriptors %>%
    write_csv(file_out('data/exports/pheno_data_with_additional_descriptors.csv')),
  
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
  
  ## Calculate margin for site differences
  site_rf_margin = site_differences_rf %>%
    margins() %>%
    summarise(Margin = mean(Margin)),
  
  ## site differences MDS and importance plots
  site_differences_mds_plot = site_differences_rf %>%
    siteDifferencesRFplot(pheno_data_with_additional_descriptors),
    
  ## calculate descriptor correction factors
  site_correction_factors = pheno_data_with_additional_descriptors %>%
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
  
  ## re-analyse site differences after site correction
  site_differences_rf_post_correction = site_corrected_analysis_suitable_data %>%
    rf(cls = pheno_data_with_additional_descriptors$Location %>% factor(),
       nreps = 100),
  
  ## Calculate margin for site differences
  site_rf_post_correction_margin = site_differences_rf_post_correction %>%
    margins() %>%
    summarise(Margin = mean(Margin)),
  
  ## run unsupervised random forest analysis
  unsupervised_rf = site_corrected_analysis_suitable_data %>%
    rf(cls = NULL,nreps = 100),
  
  ## calculate decline indexes
  decline_indexes = unsupervised_rf %>%
    calcDIs(site_corrected_pheno_data),
  
  ## export decline indexes
  export_decline_indexes = decline_indexes %>%
    write_csv(file_out('data/exports/decline_indexes.csv')),
  
  ## plot decline indexes
  decline_indexes_plot = declineIndexesPlot(decline_indexes),
  
  ## plot descriptors against decline indexes 
  descriptor_scatter_plots = site_corrected_analysis_suitable_data %>%
    descriptorScatterPlots(decline_indexes),
  
  ## find optimal random forest parameters for PDI
  PDI_rf_tune_results = tuneModel(site_corrected_analysis_suitable_data,decline_indexes$PDI),
  PDI_rf_tune_params = optimalParams(PDI_rf_tune_results),
  
  ## find optimal random forest parameters for PDI
  DAI_rf_tune_results = tuneModel(site_corrected_analysis_suitable_data,decline_indexes$DAI),
  DAI_rf_tune_params = optimalParams(DAI_rf_tune_results),
  
  ## plot tuning results
  rf_tune_plot = tuneResultsPlot(PDI_rf_tune_results,DAI_rf_tune_results),
  
  ## generate PDI predictive random forest model
  PDI_rf_model = site_corrected_analysis_suitable_data %>%
    {set.seed(1234)
      randomForest(.,y = decline_indexes$PDI,
                 ntree = PDI_rf_tune_params$ntree,
                 mtry = PDI_rf_tune_params$mtry)},
  
  ## generate DAI predictive random forest model
  DAI_rf_model = site_corrected_analysis_suitable_data %>%
    {set.seed(1234)
      randomForest(.,y = decline_indexes$DAI,
                   ntree = DAI_rf_tune_params$ntree,
                   mtry = DAI_rf_tune_params$mtry)},
  
  ## create descriptor contribution plots
  descriptor_contribution_plots = descriptorImportancePlots(PDI_rf_model,DAI_rf_model),
  
  ## PDI lime analysis
  PDI_lime_analysis = PDIlimeAnalysis(site_corrected_analysis_suitable_data,PDI_rf_model,decline_indexes),
  
  ## DAI lime analysis
  DAI_lime_analysis = DAIlimeAnalysis(site_corrected_analysis_suitable_data,DAI_rf_model,decline_indexes),
  
  ## PDI lime analysis plot
  PDI_lime_analysis_plot = PDIlimeAnalysisPlot(PDI_lime_analysis,decline_indexes),
  
  ## DAI lime analysis plot
  DAI_lime_analysis_plot = DAIlimeAnalysisPlot(DAI_lime_analysis,decline_indexes),
  
  ## PDI rf model response surfaces
  PDI_response_surfaces = PDIresponseSurfaces(PDI_rf_model,
                                              decline_indexes,
                                              site_corrected_analysis_suitable_data),
  
  ## DAI rf model response surfaces
  DAI_response_surfaces = DAIresponseSurfaces(DAI_rf_model,
                                              decline_indexes,
                                              site_corrected_analysis_suitable_data),
  
  ## render tables
  tables = render(knitr_in('manuscript/tables.Rmd'),quiet = T),
  
  ## render figures
  figures = render(knitr_in('manuscript/figures.Rmd'),quiet = T),
  
  ## render manuscript
  manuscript = render(knitr_in('manuscript/manuscript.Rmd'),quiet = T),
  
  ## render supplementary
  supplementary = render(knitr_in('manuscript/supplementary.Rmd'),quiet = T)
)