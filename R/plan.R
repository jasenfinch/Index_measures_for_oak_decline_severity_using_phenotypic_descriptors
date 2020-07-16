# Workflow plan

plan <- drake_plan(
  
  ## read in data, correct data sheet mistakes
  pheno_data = downloadData() %>%
    map(readPhenotypeSheet) %>%
    {set_names(.,map_chr(.,~{.$Location}))} %>%
    dataCorrections() %>%
    mutate(ID = 1:nrow(.)), 
  
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
  
  ## Site information table
  site_information = siteTable(),
  
  ## Analyse site differences using supervised random forest
  site_differences_rf = analysis_suitable_data %>%
    stratifiedRF(pheno_data_with_additional_descriptors$Location %>% factor(),
       n = 100),
  
  ## Calculate margin for site differences
  site_rf_margin = site_differences_rf %>%
    margins() %>%
    summarise(Margin = mean(Margin)),
  
  ## Calculate AUC for site differences
  site_rf_auc = site_differences_rf %>%
    auc(),
  
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
    stratifiedRF( pheno_data_with_additional_descriptors$Location %>% factor(),
       n = 100),
  
  ## Calculate margin for site differences post adjustment
  site_rf_post_correction_margin = site_differences_rf_post_correction %>%
    margins() %>%
    summarise(Margin = mean(Margin)),
  
  ## Calculate AUC for site differences post adjustment
  site_rf_post_correction_auc = site_differences_rf_post_correction %>%
    auc(),
  
  ## site differences post correction MDS and importance plots
  site_differences_post_correction_mds_plot = site_differences_rf_post_correction %>%
    siteDifferencesRFplot(pheno_data_with_additional_descriptors),
  
  ## run unsupervised random forest analysis
  unsupervised_rf = site_corrected_analysis_suitable_data %>%
    rf(cls = NULL,nreps = 100),
  
  ## calculate decline indexes
  decline_indexes = unsupervised_rf %>%
    calcDIs(invertPDI = TRUE,invertDAI = FALSE) %>%
    bind_cols(site_corrected_pheno_data %>%
                select(Location,ID,`Tree No`,Status,ChosenGroup)),
  
  ## export decline indexes
  export_decline_indexes = decline_indexes %>%
    write_csv(file_out('data/exports/decline_indexes.csv')),
  
  ## plot decline indexes
  decline_indexes_plot = declineIndexesPlot(decline_indexes),
  
  ## plot PDI against symptomatic and non-sympotmatic groups
  PDI_status_plot = decline_indexes %>% 
    ggplot(aes(x = Status, y = PDI)) + 
    geom_boxplot() + 
    theme_bw(),
  
  ## t-test for PDI and status
  PDI_status_ttest = decline_indexes %>%
    filter(ChosenGroup %in% c('Control','AOD','COD')) %>%
    t.test(PDI~Status,data = .,var.equal = TRUE) %>%
    tidy(),
  
  ## DAI decline type summary
  DAI_groups_summary = decline_indexes %>%
    filter(ChosenGroup %in% c('AOD','COD')) %>% 
    group_by(ChosenGroup) %>% 
    summarise(median = median(DAI),
              mean = mean(DAI)),
  
  ## Remission PDI t-test
  remission_PDI_group_ttest = decline_indexes %>%
    filter(ChosenGroup %in% c('Control','AOD','COD','Remission')) %>%
    {
      d <- .
      c_r <- d %>%
        filter(ChosenGroup %in% c('Control','Remission')) %>%
        t.test(PDI~ChosenGroup,data = .,var.equal = TRUE) %>%
        tidy()
      s_r <- d %>%
        filter(ChosenGroup %in% c('Remission','AOD','COD')) %>%
        mutate(new_status = ChosenGroup) %>%
        {
          .$new_status[.$ChosenGroup == 'AOD'] <- 'Symptomatic'
          .$new_status[.$ChosenGroup == 'COD'] <- 'Symptomatic'
          return(.)
        } %>%
        t.test(PDI~new_status,data = .,var.equal = TRUE) %>%
        tidy()
      list(`control~remission` = c_r,
           `symptomatic~remission` = s_r) %>%
        bind_rows(.id = 'comparison')
    },
  
  ## Remission DAI ttest
  remission_DAI_group_ttest = decline_indexes %>%
    filter(ChosenGroup %in% c('AOD','COD','Remission')) %>%
    {
      d <- .
      r_a <- d %>%
        filter(ChosenGroup %in% c('AOD','Remission')) %>%
        t.test(DAI~ChosenGroup,data = .,var.equal = TRUE) %>%
        tidy()
      r_c <- d %>%
        filter(ChosenGroup %in% c('COD','Remission')) %>%
        t.test(DAI~ChosenGroup,data = .,var.equal = TRUE) %>%
        tidy()
      list(`AOD~remission` = r_a,
           `COD~remission` = r_c) %>%
        bind_rows(.id = 'comparison')
    },
  
  ## plot DAI against AOD and COD groups
  DAI_groups_plot = decline_indexes %>% 
    filter(ChosenGroup %in% c('AOD','COD')) %>% 
    ggplot(aes(x = ChosenGroup, y = DAI)) + 
    geom_boxplot() + 
    theme_bw(),
  
  ## t-test for DAI and AOD and COD groups
  DAI_groups_ttest = decline_indexes %>% 
    filter(ChosenGroup %in% c('AOD','COD')) %>%
    t.test(DAI~ChosenGroup,data = .,var.equal = TRUE) %>%
    tidy(),
  
  ## plot descriptors against decline indexes 
  descriptor_scatter_plots = site_corrected_analysis_suitable_data %>%
    descriptorScatterPlots(decline_indexes),
  
  ## find optimal random forest parameters for PDI
  PDI_rf_tune_results = tuneModel(site_corrected_analysis_suitable_data,
                                  decline_indexes$PDI,
                                  index_size = 1),
  PDI_rf_tune_params = optimalParams(PDI_rf_tune_results),
  
  ## find optimal random forest parameters for PDI
  DAI_rf_tune_results = tuneModel(site_corrected_analysis_suitable_data,
                                  decline_indexes$DAI,index_size = 2),
  DAI_rf_tune_params = optimalParams(DAI_rf_tune_results),
  
  ## plot tuning results
  rf_tune_plot = tuneResultsPlot(PDI_rf_tune_results,DAI_rf_tune_results),
  
  ## generate PDI predictive random forest model
  PDI_rf_model = site_corrected_analysis_suitable_data %>%
    {set.seed(1234)
      randomForest(.,y = decline_indexes$PDI,
                   ntree = PDI_rf_tune_params$ntree,
                   mtry = PDI_rf_tune_params$mtry,
                   importance = TRUE)},
  
  ## PDI descriptor importance
  PDI_descriptor_importance = descriptorImportance(PDI_rf_model),
  
  ## generate DAI predictive random forest model
  DAI_rf_model = site_corrected_analysis_suitable_data %>%
    {set.seed(1234)
      randomForest(.,y = decline_indexes$DAI,
                   ntree = DAI_rf_tune_params$ntree,
                   mtry = DAI_rf_tune_params$mtry,
                   importance = TRUE)},
  
  ## DAI descriptor importance
  DAI_descriptor_importance = descriptorImportance(DAI_rf_model),
  
  ## create descriptor contribution plots
  descriptor_contribution_plots = descriptorImportancePlots(PDI_descriptor_importance,DAI_descriptor_importance),
  
  ## extract PDI example trees
  PDI_example_cases = PDIexampleCases(site_corrected_analysis_suitable_data,decline_indexes),
  
  ## extract DAI example trees
  DAI_example_cases = DAIexampleCases(site_corrected_analysis_suitable_data,decline_indexes),
  
  ## PDI lime analysis
  PDI_lime_analysis = PDIlimeAnalysis(site_corrected_analysis_suitable_data,PDI_rf_model,PDI_example_cases),
  
  ## DAI lime analysis
  DAI_lime_analysis = DAIlimeAnalysis(site_corrected_analysis_suitable_data,DAI_rf_model,DAI_example_cases),
  
  ## PDI lime analysis table
  PDI_lime_analysis_table = PDIlimeAnalysisTable(PDI_lime_analysis,PDI_example_cases,decline_indexes),
  
  ## PDI lime analysis plot
  PDI_lime_analysis_plot = PDIlimeAnalysisPlot(PDI_lime_analysis,PDI_example_cases,decline_indexes),
  
  ## PDI lime analysis table
  DAI_lime_analysis_table = DAIlimeAnalysisTable(DAI_lime_analysis,decline_indexes),
  
  ## DAI lime analysis plot
  DAI_lime_analysis_plot = DAIlimeAnalysisPlot(DAI_lime_analysis,decline_indexes),
  
  ## PDI rf model response surfaces
  PDI_response_surfaces = PDIresponseSurfaces(PDI_rf_model,
                                              PDI_example_cases,
                                              site_corrected_analysis_suitable_data),
  
  ## DAI rf model response surfaces
  DAI_response_surfaces = DAIresponseSurfaces(DAI_rf_model,
                                              DAI_example_cases,
                                              site_corrected_analysis_suitable_data),
  
  ## Surveyor names
  surveyors_names = surveyors(),
  
  ## render tables
  tables = render(knitr_in('manuscript/tables.Rmd'),quiet = T),
  
  ## render figures
  figures = render(knitr_in('manuscript/figures.Rmd'),quiet = T),
  
  ## render manuscript 
  manuscript = render(knitr_in('manuscript/manuscript.Rmd'),
                      quiet = T,
                      output_format = 'all'),
  
  ## render supplementary
  supplementary = render(knitr_in('manuscript/supplementary.Rmd'),quiet = T)
)
