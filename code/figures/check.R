
manuscript_check <- function(file = 'manuscript.Rmd'){
  
  options(knitr.table.format = 'markdown')
  
  cat('######### Manuscript Checks #########\n')
  cat('\nWord count')
  
  suppressMessages({
    wordcountaddin::text_stats(file) %>%
      print()
  })
  
  cat('\nSpelling\n\n')
  
  spelling::spell_check_files(file,lang = 'en_GB') %>%
    print()
  
  cat('\nStyle')
  
  gramr::write_good_file(file) %>%
    print()
}