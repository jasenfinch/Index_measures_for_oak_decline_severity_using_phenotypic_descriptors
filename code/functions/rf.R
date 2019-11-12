#' rf
#' @description perform random forest repetitions
#' @param analysisTable tibble of phenotype data suitable for random forest analysis as returned by \code{preparePhenotypeData}
#' @param cls analysisTable column to use as response vector. NULL for unsupervised analyses.
#' @param nreps number of repetitions
#' @importFrom randomForest randomForest
#' @export

rf <- function(analysisTable,cls,nreps = 100,seed = 1234){
  set.seed(seed)
  map(1:nreps,~{
    randomForest(analysisTable,cls,proximity = T)
  })
}
