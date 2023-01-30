
#' A function to transform data so it is in the proper format to be used with other functions in this package.
#'
#' @param df A dataframe where the first column contains the nodes (such as countries). The remaining columns each contain values for a single year.
#'
#' @return A dataframe with labels removed. Columns are unique years and rows are unique nodes.
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
