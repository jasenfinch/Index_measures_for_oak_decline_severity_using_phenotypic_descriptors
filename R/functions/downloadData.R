
downloadData <- function(){
  tmp_dir <- tempdir()
  data_path <- str_c(tmp_dir,'/pheno')
  dir.create(data_path)
  
  pb_download(repo = 'jasenfinch/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors',
              tag = 'data',
              dest = data_path)
  
  data_path %>%
    list.files(full.names = T)
}