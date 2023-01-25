
#' Shalizi and Thomas (2011) Contagion Test
#'
#' @param df A dataframe of values where each column is a different year, and each row is a different node
#'
#' @return A dataframe of coefficients
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
#' #running a single contagion test on the data
#' ContagionTestST(polity90)
#'
ContagionTestST <- function(df){
  #randomly assigning each observation to one of two bins
  j <- sample(c(1, 2), size = length(df[, 1]), replace = TRUE) #produces unequal bins
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
}
