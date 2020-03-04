
auc <- function(rfModels){
  rfModels %>%
    map_dbl(~{
      a <- tibble(sample = 1:length(.x$y),obs = .x$y,pred = .x$predicted,margin = margin(.x)) %>%
        bind_cols(.x$votes %>%
                    as_tibble())
      estimate <- levels(a$obs)
        a %>%
          roc_auc(obs,all_of(estimate)) %>%
          .$.estimate
    }) %>%
    mean()
}
