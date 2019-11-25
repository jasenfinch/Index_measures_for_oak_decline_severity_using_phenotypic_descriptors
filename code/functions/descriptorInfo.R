#' descriptorInfo
#' @description collate descriptor information table
#' @param analysisTable analysis suitable phenotype data tibble

descriptorInfo <- function(analysisTable){
  pt <- analysisTable %>%
    {tibble(Descriptor = names(.),`Data Type` = map_chr(.,class))} %>%
    filter(!(Descriptor %in% c('Crown condition (%)','Crown volume (m^3)','Bleed prevalence (%)','Agrilus exit hole density (m^-2)')))
  
  type <- tribble(
    ~Descriptor,~Description,~`Categories / units`,~Reference,
    'Diameter at breast height (mm)','','mm','',
    'Crown radius (m)','','m','',
    'Total height (m)','','m','',
    'Lower crown height (m)','','m','',
    'Timber height (m)','','m','',
    'Missing crown (%)','','%','',
    'Crown transparency (%)','','%','',
    'Crown contact (%)','','%','',
    'Crown mildew','','','',
    'Branch epicormics','','','',
    'Crown fruiting bodies','','','',
    'Dieback location','','','',
    'Dieback type','','','',
    'Insect defoliation','','','',
    'Insect defoliation type','','','',
    'Pruning / branch loss','','','',
    'Canopy closure','','','',
    'Social class','','','',
    'Agrilus exit holes','Number of *Agrilus* exit holes.','frequency','',
    'Stem epicormics','','','',
    'Dead stem tissue','','','',
    'Ground level fruiting bodies','','','',
    'Stem fruiting bodies','','','',
    'Oval shaped exit holes','','','',
    'Small circular shaped exit holes','','','',
    'Active bleeds','Number of active stem bleeds.','frequency','',
    'Active bleed size (mm)','','mm','',
    'Black staining','','','',
    'Black staining size (mm)','','mm','',
    'Calloused wound','','','',
    'Calloused wound size (mm)','','mm',''
  ) %>%
    mutate(`Descriptor Type` = c(rep('tree size',5),
                                 rep('crown',13),
                                 rep('stem',13)))
  
  pt <- pt %>%
    left_join(type, by = "Descriptor") %>%
    mutate(Descriptor = str_to_lower(Descriptor) %>%
             str_remove_all('\\((.*?)\\)') %>%
             str_replace_all('Agrilus','*Agrilus*')) %>%
    select(`Descriptor Type`,everything()) %>%
    arrange(`Descriptor Type`,Descriptor)
  
  pt$`Data Type`[pt$`Data Type` == 'numeric'] <- 'continous'
  pt$`Data Type`[pt$`Data Type` == 'factor'] <- 'categorical'
  
  return(pt)
}
