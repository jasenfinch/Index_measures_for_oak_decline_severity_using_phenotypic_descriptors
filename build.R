
system('docker build . -t oak_pdi')

system(paste0('export GITHUB_PAT=',Sys.getenv("GITHUB_PAT")))

system('docker run -e GITHUB_PAT -v $(pwd):/home/rstudio/Index_measures_for_oak_decline_severity_using_phenotypic_descriptors oak_pdi')
