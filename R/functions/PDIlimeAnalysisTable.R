
PDIlimeAnalysisTable <- function(PDI_lime_analysis,decline_indexes){
  cases <- tibble(case = 1:3 %>% as.character(),
                  name = c('Healthy','Moderate decline','Severe decline'),
                  PDI = c(min(decline_indexes$PDI),
                          decline_indexes$PDI[abs(decline_indexes$PDI - 0.5) == min(abs(decline_indexes$PDI - 0.5))],
                          max(decline_indexes$PDI)))
  
  PDI_lime_analysis %>%
    left_join(cases,by = 'case') %>%
    select(Status = name,PDI,`Predicted PDI` = prediction,Descriptor = feature,Value = feature_value,`Descriptor (\\textit{d}) range` = feature_desc,Weight = feature_weight) %>%
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
