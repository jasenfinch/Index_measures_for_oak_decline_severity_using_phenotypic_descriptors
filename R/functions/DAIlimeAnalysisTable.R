
DAIlimeAnalysisTable <- function(DAI_lime_analysis,decline_indexes){
  cases <- tibble(case = 1:5 %>% as.character(),
                  name = c('Neutral','Moderate AOD','Severe AOD','Moderate COD','Severe COD'),
                  DAI = c(decline_indexes %>% filter(abs(DAI) == min(abs(DAI))) %>% .$DAI,
                          decline_indexes %>% filter(abs(DAI - 0.5) == min(abs(DAI - 0.5))) %>% .$DAI,
                          decline_indexes %>% filter(DAI == max(DAI)) %>% .$DAI,
                          decline_indexes %>% filter(abs(DAI + 0.5) == min(abs(DAI + 0.5))) %>% .$DAI,
                          decline_indexes %>% filter(DAI == min(DAI)) %>% .$DAI)
  )
  
  DAI_lime_analysis %>%
    left_join(cases,by = 'case') %>%
    select(Status = name,DAI,`Predicted DAI` = prediction,Descriptor = feature,Value = feature_value,`Descriptor (\\textit{d}) range` = feature_desc,Weight = feature_weight) %>%
    {
      for (i in unique(.$Descriptor)) {
        .$`Descriptor (\\textit{d}) range` <- str_replace_all(.$`Descriptor (\\textit{d}) range`,coll(i),'\\textit{d}')
      }
      return(.)
    } %>%
    mutate_if(is.double,signif,digits = 3) %>%
    mutate(Descriptor = str_replace_all(Descriptor,coll('%'),'\\%'))
}