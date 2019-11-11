
pt <- analysisTable %>%
  {tibble(Descriptor = names(.),`Data Type` = map_chr(.,class),Description = NA,`Categories / units` = NA,Reference = NA)} %>%
  filter(!(Descriptor %in% c('Crown condition (%)','Crown volume (m^3)','Bleed prevalence (%)','Agrilus exit hole density (m^-2)')))

type <- tibble(`Descriptor Type` = c(rep('tree size',5),
                                     rep('crown',13),
                                     rep('trunk',13)),
               Descriptor = c('Diameter at breast height (cm)',
                              'Crown radius (m)',
                              'Total height (m)',
                              'Lower crown height (m)',
                              'Timber height (m)',
                              'Missing crown (%)',
                              'Crown transparency (%)',
                              'Crown contact (%)',
                              'Crown mildew',
                              'Branch epicormics',
                              'Crown fruiting bodies',
                              'Dieback location',
                              'Dieback type','Insect defoliation',
                              'Insect defoliation type',
                              'Pruning / branch loss',
                              'Canopy closure',
                              'Social class',
                              'Agrilus exit holes',
                              'Stem epicormics',
                              'Dead stem tissue',
                              'Ground level fruiting bodies',
                              'Stem fruiting bodies',
                              'Oval shaped exit holes',
                              'Small circular shaped exit holes',
                              'Active bleeds',
                              'Active bleed size (cm)',
                              'Black staining',
                              'Black staining size (cm)',
                              'Calloused wound',
                              'Calloused wound size (cm)')
)

pt <- pt %>%
  left_join(type, by = "Descriptor") %>%
  mutate(Descriptor = str_to_lower(Descriptor)) %>%
  select(`Descriptor Type`,everything()) %>%
  arrange(`Descriptor Type`,Descriptor)

pt$`Data Type`[pt$`Data Type` == 'numeric'] <- 'continous'
pt$`Data Type`[pt$`Data Type` == 'factor'] <- 'categorical'