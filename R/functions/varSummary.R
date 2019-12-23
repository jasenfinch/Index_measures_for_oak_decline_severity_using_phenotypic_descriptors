
varSummary <- function(dat){
  dat %>%
    select(Location,ID,`Diameter at breast height (m)`) %>%
    group_by(Location) %>%
    summarise(Mean = mean(`Diameter at breast height (m)`),
              SE = sd(`Diameter at breast height (m)`)/ sqrt(n()))
}