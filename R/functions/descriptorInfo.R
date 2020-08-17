#' descriptorInfo
#' @description collate descriptor information table
#' @param analysisTable analysis suitable phenotype data tibble

descriptorInfo <- function(analysisTable){
  pt <- analysisTable %>%
    {tibble(Descriptor = names(.),`Data Type` = map_chr(.,class))} %>%
    filter(!(Descriptor %in% c('Crown condition (%)','Crown volume (m^3)','Bleed prevalence (%)','Agrilus exit hole density (m^-2)',"Live crown ratio (%)","Crown surface area (m^2)","Crown production efficiency"))) %>%
    {
      .$Descriptor[.$Descriptor == 'Dead stem tissue'] <- 'Tap test'
      return(.)
    }
  
  type <- tribble(
    ~Descriptor,~Description,~`Categories / units`,
    'Diameter at breast height (m)','Diameter of the trunk to within 1mm at a height of 1.3m.','m',
    'Crown radius (m)','','m',
    'Total height (m)','Total tree height.','m',
    'Lower crown height (m)','Lowest live crown height to within 0.1m.','m',
    'Timber height (m)','','m',
    'Missing crown (%)','Percentage of missing crown.','%',
    'Crown transparency (%)','Absolute crown transparency in 5% classes.','%',
    'Crown contact (%)','','%',
    'Crown mildew','Extent of crown mildew.','0 = none; 1 = rare, only a few leaves affected; 2 = infrequent, less than 5% of leaves affected; 3 = common, manly leaves affected; 4 = abundant, most leaves affected',
    'Branch epicormics','Epicormic growth on branches.','0 = none; 1 = rare; 2 = scattered, one or two main branches with them; 3 = common, several main branches with them; 4 = abundant, majority of main branches with them',
    'Crown fruiting bodies','Presence of crown fungal fruiting bodies. Any species.','P = present; A = absent',
    'Dieback location','Main crown dieback location.','0 = none; 1 = top of the tree only; 2 = middle parts of the crown; 3 = top and middle of the crown; 4 = bottom and middle of the crown; 5 = branches at the base of the crown; 6 = throughout the crown',
    'Dieback type','Type of crown dieback.','0 = none; 1 = leaf loss only, with lateral branches partly or completely bare; 2 = dieback restricted to relatively thin branches; 3 = several large branches involved; 4 = main stem involved in the upper part of the crown; 5 = restricted to twigs',
    'Insect defoliation','Extent of leaf damage caused by insect herbivory.','0 = none; 1 = rare, only a few leaves affected; 2 = infrequent leass than 5% of leaves affected; 3 = common, many leaves affected; 4 = abundant, most leaves affected',
    'Insect defoliation type','Type of insect defoliation.','Pin = pinhole damage; Chin = chewing damage; Both = both pinhole and chewing damage',
    'Pruning / branch loss','','P = present; A = absent',
    'Canopy closure','Adjacent tree canopies in contact with canopy.','Y = yes; N = no',
    'Social class','Social class','1 = dominant; 2 = codominant; 3 = subdominant; 4 = suppressed',
    'Agrilus exit holes','Number of Agrilus biguttatus exit holes from the tree base to a height of 2m.','frequency',
    'Stem epicormics','Epicormic growth on the main stem.','0 = none; 1 = rare; 2 = scattered (11-50); 3 = common (51-100); 4 = abundant (stem totally obscured by epicormics',
    'Tap test','Main stem tapped at each cardinal point with a at a height of 1.3m to listen for hollow or solid sound to determine if that section of stem is dead or alive','H = Hollow; S = Solid',
    'Ground level fruiting bodies','Presence of ground level fungal fruiting bodies. Any species.','P = present; A = absent',
    'Stem fruiting bodies','Presence of fungal fruiting bodies on the main stem.','P = present; A = absent',
    'Oval shaped exit holes','Presence of oval shaped exit holes.','P = present; A = absent',
    'Small circular shaped exit holes','','P = present; A = absent',
    'Active bleeds','Number of active stem bleeds in 3m section from the base of the tree. Liquid running.','frequency',
    'Active bleed length (mm)','Average active bleed length in the 3m section from the base of the tree.','mm',
    'Black staining','Number of black stains in the 3m section from the base of the tree. Inactive bleeds.','fequency',
    'Black staining length (mm)','Average black stain length in the 3m section from the base of the tree.','mm',
    'Callused wound','Number of callused wounds in the 3m section from the base of the tree. Scars.','frequency',
    'Callused wound length (mm)','Average callouse wound length in the 3m section from the base of the tree.','mm',
  ) %>%
    mutate(`Descriptor Type` = c(rep('tree size',5),
                                 rep('crown',13),
                                 rep('stem',13)))
  
  pt <- pt %>%
    left_join(type, by = "Descriptor") %>%
    mutate(Descriptor = str_to_lower(Descriptor) %>%
             str_remove_all('\\((.*?)\\)') %>%
             str_replace_all('Agrilus biguttatus','\\textit{Agrilus biguttatus}')) %>%
    select(`Descriptor Type`,everything()) %>%
    arrange(`Descriptor Type`,Descriptor)
  
  pt$`Data Type`[pt$`Data Type` == 'numeric'] <- 'continuous'
  pt$`Data Type`[pt$`Data Type` == 'factor'] <- 'categorical'
  
  return(pt)
}
