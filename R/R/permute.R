#' @importFrom parallel makeCluster parLapply stopCluster detectCores
#' @importFrom purrr map_dbl
#' @export

permute <- function(dat,cls, nReps = 100, nPerm = 1000, nCores = detectCores() * 0.75, clusterType = 'PSOCK'){

  permCls <- 1:nPerm %>%
    map(~{
      sample_n(cls,nrow(cls),replace = F)
    })

  clus <- makeCluster(nCores,clusterType)

  permDist <- permCls %>%
    parLapply(cl = clus,.,function(classes,dat,nReps){
      rf(dat,classes,nReps) %>%
        map(randomForest::margin(.)) %>% mean()
    },dat = dat,nReps = nReps) %>%
    unlist()

  stopCluster(clus)

  return(permDist)
}

#' @export

permuteSignif <- function(margin,permDist){
  list(
    margin = margin,
    distribution = permDist,
    pvalue = pnorm(mean(margin),mean(permDist),sd(permDist),lower.tail = F),
    threshold = qnorm(0.05,mean(permDist),sd(permDist),lower.tail = F)
  )

}
