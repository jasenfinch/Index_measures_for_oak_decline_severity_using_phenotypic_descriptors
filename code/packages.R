# Load dependant CRAN libraries

pacman::p_load(drake,
               conflicted,
               rmarkdown,
               maps,
               mapdata,
               ggthemes,
               randomForest,
               gridExtra,
               ggrepel,
               metR,
               tidyverse,
               ggpubr,
               spelling,
               knitr,
               readxl,
               lime)

# Load dependant GitHub libraries

pacman::p_load_gh('thomasp85/patchwork',
                  'ropenscilabs/gramr',
                  'benmarwick/wordcountaddin')

# Resolve conflicts

map <- conflict_prefer('map','purrr',quiet = T)
gather <- conflict_prefer('gather','tidyr',quiet = T)
filter <- conflict_prefer('filter','dplyr',quiet = T)
margin <- conflict_prefer('margin','randomForest',quiet = T)

explain <- conflict_prefer("explain", "lime",quiet = T)