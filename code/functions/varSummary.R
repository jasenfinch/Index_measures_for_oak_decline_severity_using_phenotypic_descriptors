
varSummary <- function(dat){
  dat %>%
    select(Location,ID,`Diameter at breast height (cm)`) %>%
    group_by(Location) %>%
    summarise(Mean = mean(`Diameter at breast height (cm)`),
              SE = sd(`Diameter at breast height (cm)`)/ sqrt(n()))
}