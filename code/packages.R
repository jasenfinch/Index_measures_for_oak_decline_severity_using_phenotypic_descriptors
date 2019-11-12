# Load dependant libraries

pacman::p_load(drake,
               conflicted,
               rmarkdown,
               maps,
               mapdata,
               ggthemes,
               randomForest,
               patchwork,
               gridExtra,
               ggrepel,
               metR,
               tidyverse,
               ggpubr,
               wordcountaddin,
               spelling,
               gramr,
               knitr,
               readxl)

# Resolve conflicts

map <- conflict_prefer('map','purrr')
gather <- conflict_prefer('gather','tidyr')
filter <- conflict_prefer('filter','dplyr')
margin <- conflict_prefer('margin','randomForest')