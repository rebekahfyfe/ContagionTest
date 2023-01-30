
#' Parallelized p-value generation
#'
#' @param df A dataframe of time series values. Should be formatted such that each column contains values for a unique year. Each row is a different node.
#' @param iterations An integer denoting number of iterations to be run. Default is 100
#' @param cores An integer denoting number of cores to run the parallelization on. Default is 1.
#'
#' @return A numeric vector of p-values, length is the same as the number of iterations
#' @export
#'
#' @examples
#' #using cleaning function to get data in the proper format
#' polity <- cleaning(polityData)
#'
#' #200 iterations run on 2 cores
#' PValsParallel(df = polity, iterations = 200, cores = 2)
#'
PValsParallel <- function(df, iterations, cores = 1){
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                     type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  results <- foreach(n = 1:iterations, .combine = "rbind", .packages = "plm") %dopar% {
    binindex <- sample(c(1, 2),size = length(df[, 1]), replace = T)
    bin1 <- df[, binindex == 1]
    bin2 <- df[, binindex == 2]
    bindat <- cbind(apply(bin1, 1, mean), apply(bin2, 1, mean))
    bindat <- diff(bindat,1)
    t <- plm::purtest(bindat)
    pvals <-  t$statistic$p.value
    return(pvals)
  }
  parallel::stopCluster(myCluster)
  results2 <- sapply(results, "[[", 1)
  return(results2)
}
