#' margins
#' @description calculate class margins
#' @param rfModels list containing random forest models

margins <- function(rfModels){
  rfModels %>%
    map(~{
      d <- .
      margin(d) %>%
        tibble(Class = d$y, Margin = .) %>%
        group_by(Class) %>%
        summarise(Margin = mean(Margin),
                  .groups = 'drop')
    }) %>%
    bind_rows(.id = 'Repeat') %>%
    group_by(Class) %>%
    summarise(Margin = mean(Margin) %>%
                round(3),
              .groups = 'drop')
}
