
#' Run a parallelized network contagion test
#'
#' @param df A dataframe with a column of nodes, a column of years, and a column of values
#' @param iterations An integer denoting how many iterations of the test should be run
#' @param cores An integer denoting how many cores to use, default is 1
#' @param stationary A binary indicator for whether df contains stationary data. Default is FALSE.
#'
#' @return A dataframe with the coefficients from the contagion test
#' @export
#'
#' @examples
#' #running parallelized contagion tests on the data
#' ParallelContagionTest(df = polity90, iterations = 10000, cores = 3, stationary = FALSE)
#'
ParallelContagionTest <- function(df, iterations, cores = 1, stationary = FALSE){
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                     type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  results <- foreach(n = 1:iterations, .combine = "rbind") %dopar% {
    x = try(plm::purtest(df), silent = T)
    if(class(x) == "try-error" | stationary == FALSE){
      #for nonstationary data
      j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
      Yj1 <- df[, j == 1]
      Yj2 <- df[, j == 2]

      j1mean <- apply(Yj1, 1, mean)
      j2mean <- apply(Yj2, 1, mean)
      j1mean <- diff(j1mean, 1)   # taking the difference between Y1 and Y0 for stationarity
      j2mean <- diff(j2mean, 2)   # taking the difference between Y1 and Y0 for stationarity
      j1mean.t <- j1mean[2:(length(j1mean))]
      j2mean.t <- j2mean[2:(length(j2mean))]

      j1mean.tm1 = j1mean[1:(length(j1mean)-1)]
      j2mean.tm1 = j2mean[1:(length(j2mean)-1)]

      regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                     c(j2mean.tm1, j1mean.tm1))
      return(coefficients(regmod))
    } else{
      #for stationary data
      j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
      Yj1 <- df[, j == 1]
      Yj2 <- df[, j == 2]

      j1mean <- apply(Yj1, 1, mean)
      j2mean <- apply(Yj2, 1, mean)

      j1mean.t <- j1mean[2:(length(j1mean))]
      j2mean.t <- j2mean[2:(length(j2mean))]

      j1mean.tm1 = j1mean[1:(length(j1mean)-1)]
      j2mean.tm1 = j2mean[1:(length(j2mean)-1)]

      regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                     c(j2mean.tm1, j1mean.tm1))
      return(coefficients(regmod))
    }

  }
  parallel::stopCluster(myCluster)
  return(results)

}
