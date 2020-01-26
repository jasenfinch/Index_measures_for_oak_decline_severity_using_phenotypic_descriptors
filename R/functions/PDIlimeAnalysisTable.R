
PDIlimeAnalysisTable <- function(PDI_lime_analysis,PDI_example_cases,decline_indexes){
  IDs <- PDI_example_cases %>%
    names() %>%
    str_split_fixed('-',2) %>%
    .[,2] %>%
    as.numeric()
  
  name <- PDI_example_cases %>%
    names() %>%
    str_split_fixed('-',2) %>%
    .[,1]
  
  cases <- tibble(case = 1:3 %>% as.character(),
                  name = name,
                  PDI = decline_indexes %>%
                    filter(ID %in% IDs) %>%
                    arrange(PDI) %>%
                    .$PDI) %>%
    mutate(PDI = signif(PDI,3))
  
  PDI_lime_analysis %>%
    left_join(cases,by = 'case') %>%
    select(Status = name,PDI,`Predicted PDI` = prediction,Descriptor = feature,`Descriptor (\\textit{d}) range` = feature_desc,Weight = feature_weight) %>%
    {
      for (i in unique(.$Descriptor)) {
        .$`Descriptor (\\textit{d}) range` <- str_replace_all(.$`Descriptor (\\textit{d}) range`,coll(i),'\\textit{d}')
      }
      return(.)
    } %>%
    mutate_if(is.double,signif,digits = 3) %>%
    mutate(Descriptor = str_replace_all(Descriptor,coll('m^3'),'m\\textsuperscript{3}'),
           Descriptor = str_replace_all(Descriptor,coll('m^2'),'m\\textsuperscript{2}'),
           Descriptor = str_replace_all(Descriptor,coll('%'),'\\%'))
}
