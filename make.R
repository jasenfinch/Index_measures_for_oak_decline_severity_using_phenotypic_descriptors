
source('code/packages.R')

'code/functions/' %>%
  list.files(full.names = T) %>%
  walk(source)

source('code/plan.R')

make(plan)
