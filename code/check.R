
check <- function(file = 'manuscript.Rmd'){
  
  cat('\nWord count')
  
  suppressMessages({
    wordcountaddin::text_stats(file) %>%
      print()
  })
  
  cat('\nSpelling')
  
  spelling::spell_check_files(file,lang = 'en_GB') %>%
    print()
  
  cat('\nGrammar')
  
  gramr::write_good_file(file) %>%
    print()
}