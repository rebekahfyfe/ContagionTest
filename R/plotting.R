
#' Plots a density graph with the results from the split-halves test
#'
#' @param model A dataframe with three columns that contain the results of the split-halves test
#' @param n Number of iterations through split-halves test. Should be the same as the 'iterations' value input to lag_pc_test().
#' @param mean.xcoord The x-coordinate for where the mean value should be displayed on the graph. Default is 0.
#' @param mean.ycoord The y-coordinate for where the mean value should be displayed on the graph. Default is 1.
#' @param pval.xcoord The x-coordinate for where the p-value should be displayed on the graph. Default is 0.
#' @param pval.ycoord The y-coordinate for where the p-value should be displayed on the graph. Default is 0.
#' @param title The title to be displayed on the plot. Default is an empty string.
#' @param fillCol The fill color for the graph. Default is "lightsteelblue"
#' @param colorCol The outline color for the graph. Default is "lightsteelblue4"
#'
#' @return
#' @export
#'
#' @examples
density_graph <- function(model, n, mean.xcoord = 0, mean.ycoord = 1,
                          pval.xcoord = 0, pval.ycoord = 0, title = "",
                          fillCol = "lightsteelblue", colorCol = "lightsteelblue4"){
  require(ggplot2)
  model <- as.data.frame(model)
  # Mean of the results from the n iterations of the SH test
  mean <- mean(model[, 3])
  mean <- round(mean, digits = 3)
  # Significance of the signal, proportion of means less than 0
  pval <- sum(model[, 3] < 0) / n  ## pvalue
  pval <- round(pval, digits = 3)
  ggplot(model, aes(x = model[, 3])) +
    xlab("Contagion Signal") +
    ylab("Density") +
    ggtitle(title) +
    theme_light() +
    geom_density(fill = fillCol, color = colorCol) +
    geom_vline(aes(xintercept = mean), colour = colorCol) +
    annotate("text", x = (mean.xcoord), y = mean.ycoord,
             label = paste("Mean:", round(mean, 4)), color = "gray20") +
    geom_vline(aes(xintercept = 0),color = "gray20", linetype = "dashed") +
    annotate("text", x = (pval.xcoord), y = pval.ycoord,
             label = paste("p-value:", pval), color = "gray20")
}
