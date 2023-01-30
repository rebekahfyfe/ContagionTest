
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
  colnames(df) <- c("node", "year", "value")
  nodes <- table(df$node)
  totalYear <- length(unique(df$year))
  subsetDF <- df[(df$node %in% names(nodes[nodes == totalYear])), ]
  subsetDF <- spread(subsetDF, year, value)
  return(subsetDF)
}

