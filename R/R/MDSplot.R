#' MDSplot
#' @description Plot mulitidimensional scaling plot of random forest proximities.
#' @param rfModels list containing random forest models
#' @param phenoData tibble containing phenotype data to use for plotting
#' @param title plot title
#' @param colour phenoData column to use to map point colour aesthetics
#' @param shape phenoData column to use to map point shape aesthetics
#' @importFrom stats cmdscale
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom ggplot2 ggplot aes element_text geom_point aes_string theme_bw theme ggtitle guides guide_legend
#' @importFrom ggthemes scale_colour_ptol
#' @export

MDSplot <- function(rfModels,phenoData,title = '', colour = NULL, shape = NULL){

   proxMDS <- rfModels %>%
     mds() %>%
    bind_cols(select(phenoData,Location,`Tree No`,Status))

  ggplot(proxMDS,aes(x = `Dimension 1`,y = `Dimension 2`)) +
    geom_point(aes_string(colour = colour,shape = shape)) +
    scale_colour_ptol() +
    theme_bw(base_size = 9) +
    theme(legend.position = 'bottom',
          legend.title = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold')) +
    labs(title = title,
         x = 'Dimension 1',
         y = 'Dimension 2') +
    guides(colour = guide_legend(title.position = 'top',ncol = 2),
           shape = guide_legend(title.position = 'top',ncol = 1)) +
    coord_fixed()
}

#' mds
#' @description perform multidimensional scaling of random forest proximities
#' @param rfModels list containing random forest models
#' @export

mds <- function(rfModels){
  rfModels %>%
    map(~{.$proximity %>%
        as_tibble() %>%
        rowid_to_column(var = 'Sample1') %>%
        gather('Sample2','Proximity',-Sample1)}) %>%
    bind_rows(.id = 'Iteration') %>%
    mutate(Sample2 = as.numeric(Sample2)) %>%
    group_by(Sample1,Sample2) %>%
    summarise(Proximity = mean(Proximity)) %>%
    spread(Sample2,Proximity) %>%
    tbl_df() %>%
    select(-Sample1) %>%
    as.matrix() %>%
    {1 - .} %>%
    cmdscale() %>%
    as_tibble() %>%
    rename(`Dimension 1` = V1,`Dimension 2` = V2)
}
