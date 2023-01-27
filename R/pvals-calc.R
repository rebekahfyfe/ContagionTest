
#' Parallelized p-value generation
#'
#' @param df A dataframe of values
#' @param iterations An integer denoting number of iterations
#' @param cores An integer denoting number of cores to run the parallelization on
#'
#' @return A numeric vector of p-values
#' @export
#'
#' @examples
PValsParallel <- function(df, iterations, cores = 1){
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                    type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  results <- foreach(n = 1:iterations, .combine = "rbind", .packages = "punitroots") %dopar% {
    binindex <- sample(c(1, 2),size = length(df[, 1]), replace = T)
    bin1 <- df[, binindex == 1]
    bin2 <- df[, binindex == 2]
    bindat <- cbind(apply(bin1, 1, mean), apply(bin2, 1, mean))
    bindat <- diff(bindat,1)
    t <- punitroots::pCADFtest(bindat, crosscorr = 0.1)
    pvals <-  t$p.value
    return(pvals)
  }
  parallel::stopCluster(myCluster)
  results2 <- sapply(results, "[[", 1)
  return(results2)
}
