# Restore package cache
renv::restore()

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
               lime,
               patchwork,
               broom,
               glue,
               kableExtra,
               grid,
               piggyback,
               yardstick,
               bookdown,
               install = FALSE)

# Load dependant GitHub libraries

pacman::p_load_gh('jasenfinch/pdi',
                  'ropenscilabs/gramr',
                  'benmarwick/wordcountaddin',
                  'wilkelab/ggtext',
                  install = FALSE)

# Resolve conflicts

conflict_prefer('map','purrr',quiet = T)
conflict_prefer('gather','tidyr',quiet = T)
conflict_prefer('filter','dplyr',quiet = T)
conflict_prefer('margin','randomForest',quiet = T)
conflict_prefer("explain", "lime",quiet = T)
conflict_prefer("rf", "pdi",quiet = T)