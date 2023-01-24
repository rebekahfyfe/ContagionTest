
#' Run a parallelized network contagion test
#'
#' @param df A dataframe with a column of nodes, a column of years, and a column of values
#' @param iterations An integer denoting how many iterations of the test should be run
#' @param cores An integer denoting how many cores to use
#'
#' @return A dataframe
#' @export
#'
#' @examples
ParallelContagionTest <- function(df, iterations, cores){
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                     type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  results <- foreach(n = 1:iterations, .combine = "rbind") %do% ContagionTestST(df)
  parallel::stopCluster(myCluster)
  return(results)

}
