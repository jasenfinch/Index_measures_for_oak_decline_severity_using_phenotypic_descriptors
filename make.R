
source('R/packages.R')

'R/functions/' %>%
  list.files(full.names = T) %>%
  walk(source)

source('R/plan.R')

make(plan)
