
#' A function to transform data so it is in the proper format to be used with other functions in this package.
#'
#' @param df A dataframe where the first column contains the nodes (such as countries). The remaining columns each contain values for a single year.
#'
#' @return A dataframe with labels removed. Columns are unique nodes and rows are unique years.
#' @export
#'
#' @examples
#' df <- cleaning(polityData)
STFormat <- function(df){
  subsetDF <- as.data.frame(t(as.matrix(df)))
  subsetDF <- subsetDF[-1, ]
  subsetDF <- apply(subsetDF, 2, as.numeric)
  return(subsetDF)
}


#' Subset data so only nodes with observations in every year are included
#'
#' @param df A dataframe with three columns denoting nodes, years, and values, respectively
#'
#' @return A dataframe with three columns (node, year, and value)
#' @export
#'
#' @examples
#' allYears(polityData)
#'
allYears <- function(df){
  require(tidyr)
  colnames(df) <- c("node", "year", "value")
  nodes <- table(df$node)
  totalYear <- length(unique(df$year))
  subsetDF <- df[(df$node %in% names(nodes[nodes == totalYear])), ]
  subsetDF <- spread(subsetDF, year, value)
  return(subsetDF)
}