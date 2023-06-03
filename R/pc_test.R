
#' Run a parallelized network contagion test
#'
#' @param df A dataframe with a column of nodes, a column of years, and a column of values
#' @param iterations An integer denoting how many iterations of the test should be run
#' @param cores An integer denoting how many cores to use, default is 1
#' @param difference A binary indicator for whether df should be differenced. Default is TRUE.
#' @param threshold A threshold value for conducting a unit root test. If conducting a unit root test on df returns a p-value less than this value, the data will be differenced. Default is o.1.
#'
#' @return A dataframe with the coefficients from the contagion test
#' @export
#'
#' @examples
#' #running parallelized contagion tests on the data
#' ParallelContagionTest(df = polity90, iterations = 10000, cores = 3, stationary = FALSE)
#'
pc_test <- function(df, iterations, cores = 1, difference = TRUE, threshold = 0.1){
  require(foreach)
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                     type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  printStat <- 0
  results <- foreach(n = 1:iterations, .combine = "rbind") %dopar% {
    if(difference == TRUE){
      node_sd <- apply(df, 2, sd, na.rm = T)
      x <- plm::purtest(df[, node_sd > 0], test = "hadri", exo = "intercept")
      if(x$statistic$p.value < threshold){
        #for nonstationary data
        j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
        Yj1 <- df[, j == 1]
        Yj2 <- df[, j == 2]

        j1mean <- apply(Yj1, 1, mean, na.rm = T)
        j2mean <- apply(Yj2, 1, mean, na.rm = T)
        j1mean <- diff(j1mean, 1)   # taking the difference between Y1 and Y0 for stationarity
        j2mean <- diff(j2mean, 1)   # taking the difference between Y1 and Y0 for stationarity
        j1mean.t <- j1mean[2:(length(j1mean))]
        j2mean.t <- j2mean[2:(length(j2mean))]

        j1mean.tm1 = j1mean[1:(length(j1mean)-1)]
        j2mean.tm1 = j2mean[1:(length(j2mean)-1)]

        regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                     c(j2mean.tm1, j1mean.tm1))
        results <- coefficients(regmod)
        returnList <- append(results, printStat, after = 0)
        return(returnList)
      } else{
          printStat <- 1
          #for stationary data
          j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
          Yj1 <- df[, j == 1]
          Yj2 <- df[, j == 2]

          j1mean <- apply(Yj1, 1, mean, na.rm = T)
          j2mean <- apply(Yj2, 1, mean, na.rm = T)

          j1mean.t <- j1mean[2:(length(j1mean))]
          j2mean.t <- j2mean[2:(length(j2mean))]

          j1mean.tm1 = j1mean[1:(length(j1mean)-1)]
          j2mean.tm1 = j2mean[1:(length(j2mean)-1)]

          regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                       c(j2mean.tm1, j1mean.tm1))
          results <- coefficients(regmod)
          returnList <- append(results, printStat, after = 0)
          return(returnList)
          #return(coefficients(regmod))
      }
    } else{
      printStat <- 1
      #for stationary data
      j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
      Yj1 <- df[, j == 1]
      Yj2 <- df[, j == 2]

      j1mean <- apply(Yj1, 1, mean, na.rm = T)
      j2mean <- apply(Yj2, 1, mean, na.rm = T)

      j1mean.t <- j1mean[2:(length(j1mean))]
      j2mean.t <- j2mean[2:(length(j2mean))]

      j1mean.tm1 = j1mean[1:(length(j1mean)-1)]
      j2mean.tm1 = j2mean[1:(length(j2mean)-1)]

      regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                     c(j2mean.tm1, j1mean.tm1))
      results <- coefficients(regmod)
      returnList <- append(results, printStat, after = 0)
      return(returnList)
      #return(coefficients(regmod))
    }
  }
  parallel::stopCluster(myCluster)
  ifelse(results[[1]] == 0, print("Took 1st difference"), print("Did not take 1st difference"))
  results <- results[, -1]
  return(results)

}
