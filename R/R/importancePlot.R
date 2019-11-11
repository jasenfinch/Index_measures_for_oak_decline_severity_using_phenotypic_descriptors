#' importancePlot
#' @description plot random forest Gini importance values
#' @param rfModels list containing random forest models
#' @param phenoData tibble containing phenotype data to use for plotting
#' @param title plot title
#' @importFrom randomForest importance
#' @importFrom dplyr arrange
#' @importFrom ggplot2 coord_flip xlab labs
#' @importFrom ggthemes ptol_pal
#' @importFrom stringr str_c
#' @export

importancePlot <- function(rfModels,phenoData,title = ''){
  imp <- rfModels %>%
    map(~{
      imp <- importance(.)
      feat <- rownames(imp)
      imp <- imp[,1]
      tibble(Feature = feat,`Gini Importance` = imp)
    }) %>%
    bind_rows(.id = 'Repeat') %>%
    group_by(Feature) %>%
    summarise(`Gini Importance` = mean(`Gini Importance`)) %>%
    arrange(`Gini Importance`) %>%
    mutate(Feature = factor(Feature,levels = Feature))

  ggplot(imp,aes(x = Feature,y = `Gini Importance`)) +
    geom_point(colour = ptol_pal()(1)) +
    coord_flip() +
    theme_bw() +
    ggtitle(title) +
    theme(plot.title = element_text(face = 'bold',size = 9),
          axis.title = element_text(face = 'bold')) +
    labs(caption = str_c('Scores averaged across ',length(rfModels),' repetitions'),
         y = 'Gini importance',
         x = 'Descriptor')
}
