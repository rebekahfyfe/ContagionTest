
#' Run a parallelized network contagion test
#'
#' @param df A dataframe with a column of nodes, a column of years, and a column of values
#' @param iterations An integer denoting how many iterations of the test should be run
#' @param cores An integer denoting how many cores to use, default is 1
#'
#' @return A dataframe with the coefficients from the contagion test
#' @export
#'
#' @examples
#' # Transposing polityData to fit correct format (a column for each year, a row for each node)
#' polity90 <- as.data.frame(t(as.matrix(polityData)))
#' # getting rid of the names
#' politynames <- polity90[1,]
#' polity90 <- polity90[-1,]
#' #converting all values to numeric
#' polity90 <- apply(polity90, 2, as.numeric)
#' #running parallelized contagion tests on the data
#' ParallelContagionTest(df = polity90, iterations = 10000, cores = 3)
#'
ParallelContagionTest <- function(df, iterations, cores = 1){
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                     type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  results <- foreach(n = 1:iterations, .combine = "rbind") %do% ContagionTestST(df)
  parallel::stopCluster(myCluster)
  return(results)

}
