
varSummary <- function(dat){
  dat %>%
    select(Location,ID,`Diameter at breast height (mm)`) %>%
    group_by(Location) %>%
    summarise(Mean = mean(`Diameter at breast height (mm)`),
              SE = sd(`Diameter at breast height (mm)`)/ sqrt(n()))
}