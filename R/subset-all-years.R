#function for subsetting data so that only observations with every year are included
#data frame being passed must include 2 columns (first describing the nodes, the second being a "year" column)
allYears <- function(df){
  colnames(df) <- c("node", "year")
  nodes <- table(df$node)
  totalYear <- length(unique(df$year))
  subsetDF <- df[(df$node %in% names(nodes[nodes == totalYear])), ]
  return(subsetDF)
}
