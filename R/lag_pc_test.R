
#' Run a parallelized network contagion test
#'
#' @param df A dataframe with a column of nodes, a column of years, and a column of values
#' @param iterations An integer denoting how many iterations of the test should be run
#' @param cores An integer denoting how many cores to use, default is 1
#' @param difference A binary indicator for whether df should be differenced. Default is TRUE.
#' @param threshold A threshold value for conducting a unit root test. If conducting a unit root test on df returns a p-value less than this value, the data will be differenced. Default is 0.1.
#' @param lagWin An integer indicating the lag window to be used. All the values within that window are averaged to get a single value. Default is 1.
#' @param missingData A binary indicator of whether 'df' includes missing data. If set to TRUE, ndiffs() will be used to test for stationarity.
#' @param unitTest A binary indicator of whether the function is being run in a unit test. Default is FALSE and should rarely be set to TRUE.
#'
#' @return A dataframe with the coefficients from the contagion test
#' @export
#'
#' @examples
#' # converting example data to the proper format -> nodes as columns, time periods as rows
#' polity <- allTime(polityData)
#' polity <- STFormat(polity)
#' # running parallelized contagion tests on the data
#' lag_pc_test(df = polity, iterations = 10000, cores = 3, difference = TRUE, threshold = 0.1, lagWin = 1, missingData = FALSE, unitTest = FALSE)
#'
lag_pc_test <- function(df, iterations = 1000, cores = 1, difference = TRUE, threshold = 0.1, lagWin = 1, missingData = FALSE, unitTest = FALSE){
  require(foreach)
  require(forecast)
  require(doRNG)
  myCluster <- parallel::makeCluster(cores, # number of cores to use
                                     type = "PSOCK") # type of cluster
  doParallel::registerDoParallel(myCluster)
  lagWin <- lagWin
  printStat <- 0
  # when there is missing data, use ndiffs() to determine whether differencing is necessary
  if(missingData == TRUE){
    numDif <- mean(apply(df, 2, FUN = forecast::ndiffs))
  }
  results <- foreach(n = 1:iterations, .combine = "rbind",
                     .export = c("diff_data", "no_diff_data", "lag_calc"),
                     .options.RNG = 39) %dorng% {
    if(missingData == TRUE){
      if(numDif < 0.5){ #if less than half of the columns require differencing, do not difference the data
        returnList <- no_diff_data(df, lagWin)
        return(returnList)
      }
      else{ #difference the data otherwise
        #for nonstationary data
        returnList <- diff_data(df, lagWin)
        return(returnList)
      }
    }
    else{
      if(difference == TRUE){
        # if there is not missing data, use purtest() to determine whether differencing is necessary
        node_sd <- apply(df, 2, sd, na.rm = T)
        prop_na <- function(x){sum(is.na(x))/length(x)}
        x <- plm::purtest(df[, node_sd > 0 & apply(df,2,prop_na) == 0.0], test = "hadri", exo = "intercept")

        if(x$statistic$p.value < threshold){
          # for nonstationary data
          returnList <- diff_data(df, lagWin)
          return(returnList)

        } else{
          # for stationary data
          returnList <- no_diff_data(df, lagWin)
          return(returnList)
        }

      } else{
        # for stationary data
        returnList <- no_diff_data(df, lagWin)
        return(returnList)
      }
    }
  }
  parallel::stopCluster(myCluster)
  # print statement telling whether data was differenced
  ifelse(results[[1]] == 1, print("Took 1st difference"), print("Did not take 1st difference"))
  # when running unit test, do not remove printStat value
  if(unitTest == TRUE){
    if(iterations == 1){
      return(results)
    }
    else{
      return(results[1,])
    }
  }
  else{ #remove printStat value when not running unit test
  if(iterations == 1){
    results <- results[-1]
    return(results)
  }
  else{
    results <- results[,-1]
    return(results)
  }
  }
}


no_diff_data <- function(df, lagWin = 1){
  printStat <- 0
  #for stationary data
  j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
  #splitting data into two random halves
  Yj1 <- df[, j == 1]
  Yj2 <- df[, j == 2]

  #calculate mean at each time period
  j1mean <- apply(Yj1, 1, mean, na.rm = T)
  j2mean <- apply(Yj2, 1, mean, na.rm = T)

  #subset starting at time t
  j1mean.t <- j1mean[(lagWin+1):(length(j1mean))]
  j2mean.t <- j2mean[(lagWin+1):(length(j2mean))]

  #subset starting at time t - 1
  j1mean.tm1 <- lag_calc(j1mean, lagWin)
  j2mean.tm1 <- lag_calc(j2mean, lagWin)

  #estimate relationship between two halves
  regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                 c(j2mean.tm1, j1mean.tm1))
  results <- coefficients(regmod)
  returnList <- append(results, printStat, after = 0)
  return(returnList)
}


diff_data <- function(df, lagWin = 1) {
  printStat <- 1
  #for nonstationary data
  j <- sample(c(1, 2), size = ncol(df), replace = TRUE) #produces unequal bins
  #split data into 2 random halves
  Yj1 <- df[, j == 1]
  Yj2 <- df[, j == 2]

  #calculate mean at each time period
  j1mean <- apply(Yj1, 1, mean, na.rm = T)
  j2mean <- apply(Yj2, 1, mean, na.rm = T)

  j1mean <- diff(j1mean, 1)   # taking the difference between Y1 and Y0 for stationarity
  j2mean <- diff(j2mean, 1)   # taking the difference between Y1 and Y0 for stationarity

  #subset starting at time t
  j1mean.t <- j1mean[(lagWin+1):(length(j1mean))]
  j2mean.t <- j2mean[(lagWin+1):(length(j2mean))]

  #subset starting at time t - 1
  j1mean.tm1 <- lag_calc(j1mean, lagWin)
  j2mean.tm1 <- lag_calc(j2mean, lagWin)


  #estimate relationship between two halves
  regmod <- lm(c(j1mean.t, j2mean.t) ~ c(j1mean.tm1, j2mean.tm1) +
                 c(j2mean.tm1, j1mean.tm1))
  results <- coefficients(regmod)
  returnList <- append(results, printStat, after = 0)
  return(returnList)
}


lag_calc <- function(df, lagWin){
  indexes <- (lagWin + 1):length(df)
  #take mean of all time periods in lag window
  laggedDf <- sapply(indexes, function(i) {
    mean(df[(i-1):(i-lagWin)]) } )
  return(laggedDf)
  }

