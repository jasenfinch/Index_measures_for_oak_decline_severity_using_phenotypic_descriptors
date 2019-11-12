#' preparePhenotypeData
#' @description process parsed phenotype data sheets into a tibble suitable for random forest analysis
#' @param phenotypeData parsed phenotype data collection sheet returned from \code{readPhenotypeSheet}

preparePhenotypeData <- function(phenotypeData){
  description <- phenotypeData %>%
      .$Description %>%
    spread(Descriptor,Value)

  cardinalAssessments <- phenotypeData %>%
      .$CardinalAssessments %>%
    filter(!(Descriptor %in% c('Whole stem photo ref number',
                               '3m section photo ref number')
    )) %>%
    spread(Descriptor,Value)

  averageCrownRadius <- cardinalAssessments %>%
    group_by(ID) %>%
    mutate(`Crown radius` = `Crown radius` %>% as.numeric()) %>%
    summarise(`Average Crown radius` = mean(`Crown radius`))

  deadTissue <- cardinalAssessments %>%
    group_by(ID) %>%
    summarise(`Dead Stem Tissue` = 'Hollow' %in% `Tap test`)
  deadTissue$`Dead Stem Tissue`[deadTissue$`Dead Stem Tissue` == T] <- 'present'
  deadTissue$`Dead Stem Tissue`[deadTissue$`Dead Stem Tissue` == F] <- 'absent'

  canopyClosure <- cardinalAssessments %>%
    group_by(ID) %>%
    summarise(`Canopy Closure` = 'Y' %in% `Canopy closure`)
  canopyClosure$`Canopy Closure`[canopyClosure$`Canopy Closure` == T] <- 'present'
  canopyClosure$`Canopy Closure`[canopyClosure$`Canopy Closure` == F] <- 'absent'

  symptoms <- phenotypeData %>%
      .$Symptoms
  symptoms$Size[is.na(symptoms$Size)] <- 0

  crackFrequencies <- symptoms
  crackFrequencies$Size[crackFrequencies$Size > 0] <- 1

  crackFrequencies <- crackFrequencies %>%
    group_by(ID,`Symptom Type`) %>%
    summarise(Count = sum(Size)) %>%
    spread(`Symptom Type`,Count) %>%
    tbl_df() %>%
    mutate(ID = as.numeric(ID))


  averageCrackSize <- symptoms %>%
    group_by(ID,`Symptom Type`) %>%
    summarise(`Average Crack Size` = mean(Size,na.rm = T)) %>%
    spread(`Symptom Type`,`Average Crack Size`) %>%
    rename(`Average Active Bleed Size` = `Active bleeds`,
           `Average Black Staining Size` = `Black staining`,
           `Average Calloused Wound Size` = `Calloused wound`) %>%
    tbl_df() %>%
    mutate(ID = as.numeric(ID))

  description <- description %>%
    left_join(deadTissue, by = "ID") %>%
    left_join(averageCrownRadius, by = "ID") %>%
    left_join(canopyClosure, by = "ID") %>%
    left_join(crackFrequencies, by = "ID") %>%
    left_join(averageCrackSize, by = "ID")

  description$`Insect defoliation type` <- tolower(description$`Insect defoliation type`)

  description$`Crown fruiting bodies`[description$`Crown fruiting bodies` == 'P'] <- 'present'
  description$`Crown fruiting bodies`[description$`Crown fruiting bodies` == 'A'] <- 'absent'
  description$`Pruning / branch loss`[description$`Pruning / branch loss` == 'P'] <- 'present'
  description$`Pruning / branch loss`[description$`Pruning / branch loss` == 'A'] <- 'absent'
  description$`Ground level fruiting bodies`[description$`Ground level fruiting bodies` == 'P'] <- 'present'
  description$`Ground level fruiting bodies`[description$`Ground level fruiting bodies` == 'A'] <- 'absent'
  description$`Stem  fruiting bodies`[description$`Stem  fruiting bodies` == 'P'] <- 'present'
  description$`Stem  fruiting bodies`[description$`Stem  fruiting bodies` == 'A'] <- 'absent'
  description$`Oval shaped exit holes`[description$`Oval shaped exit holes` == 'P'] <- 'present'
  description$`Oval shaped exit holes`[description$`Oval shaped exit holes` == 'A'] <- 'absent'
  description$`Small circular shaped exit holes`[description$`Small circular shaped exit holes` == 'P'] <- 'present'
  description$`Small circular shaped exit holes`[description$`Small circular shaped exit holes` == 'A'] <- 'absent'

  description$`Active bleeds`[is.na(description$`Active bleeds`)] <- 0
  description$`Black staining`[is.na(description$`Black staining`)] <- 0
  description$`Calloused wound`[is.na(description$`Calloused wound`)] <- 0
  description$`Average Active Bleed Size`[is.na(description$`Average Active Bleed Size`)] <- 0
  description$`Average Black Staining Size`[is.na(description$`Average Black Staining Size`)] <- 0
  description$`Average Calloused Wound Size`[is.na(description$`Average Calloused Wound Size`)] <- 0
  description$`Agrillus exit holes`[is.na(description$`Agrillus exit holes`)] <- 0

  description <- description %>%
    mutate(`Agrillus exit holes` = `Agrillus exit holes` %>% as.numeric(),
           `Crown density` = `Crown density` %>% as.numeric(),
           Dbh = Dbh %>% as.numeric(),
           `Lower Crown Ht` = `Lower Crown Ht` %>% as.numeric(),
           `Missing crown` = `Missing crown` %>% as.numeric(),
           `Timber Ht` = `Timber Ht` %>% as.numeric(),
           `Total Ht` = `Total Ht` %>% as.numeric(),
           `Average Crown radius` =`Average Crown radius` %>%  as.numeric(),
           `Active bleeds` = `Active bleeds` %>% as.numeric(),
           `Black staining` = `Black staining` %>% as.numeric(),
           `Calloused wound` = `Calloused wound` %>% as.numeric(),
           `Average Active Bleed Size` = `Average Active Bleed Size`  %>% as.numeric(),
           `Average Black Staining Size` = `Average Black Staining Size` %>% as.numeric(),
           `Average Calloused Wound Size` = `Average Calloused Wound Size` %>% as.numeric(),
           `Crown contact % of crown circumference` = `Crown contact % of crown circumference` %>% as.numeric()
    )

  description <- description %>%
    rename(Status = Symptomatic,
           `Crown transparency (%)` =  `Crown density`,
           `Diameter at breast height (cm)` = `Dbh`,
           `Lower crown height (m)` = `Lower Crown Ht`,
           `Missing crown (%)` = `Missing crown`,
           `Timber height (m)` = `Timber Ht`,
           `Total height (m)` = `Total Ht`,
           `Crown radius (m)` = `Average Crown radius`,
           `Active bleed size (cm)` = `Average Active Bleed Size`,
           `Black staining size (cm)` = `Average Black Staining Size` ,
           `Calloused wound size (cm)` = `Average Calloused Wound Size`,
           `Agrilus exit holes` = `Agrillus exit holes`,
           `Stem fruiting bodies` = `Stem  fruiting bodies`,
           `Canopy closure` = `Canopy Closure`,
           `Social class` = `Social Class`,
           `Dead stem tissue` = `Dead Stem Tissue`,
           `Crown contact (%)` = `Crown contact % of crown circumference`)

  description$Status[description$Status == 'Y'] <- 'Symptomatic'
  description$Status[description$Status == 'N'] <- 'Non-symptomatic'
  return(description)
}
