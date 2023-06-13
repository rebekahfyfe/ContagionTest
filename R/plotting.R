
#' Plots a density graph with the results from the split-halves test
#'
#' @param model A dataframe with three columns that contain the results of the split-halves test
#' @param n Number of iterations through split-halves test. Should be the same as the 'iterations' value input to lag_pc_test().
#' @param mean.xcoord The x-coordinate for where the mean value should be displayed on the graph. Default is 0.
#' @param mean.ycoord The y-coordinate for where the mean value should be displayed on the graph. Default is 1.
#' @param pval.xcoord The x-coordinate for where the p-value should be displayed on the graph. Default is 0.
#' @param pval.ycoord The y-coordinate for where the p-value should be displayed on the graph. Default is 0.
#'
#' @return
#' @export
#'
#' @examples
density_graph <- function(model, n, mean.xcoord = 0, mean.ycoord = 1,
                          pval.xcoord = 0, pval.ycoord = 0, title = ""){
  require(ggplot2)
  model <- as.data.frame(model)
  mean <- mean(model[, 3]) ## input this in the plot below
  mean <- round(mean, digits = 3)
  # Significance of the signal, proportion of means less than 0
  pval <- sum(model[, 3] < 0) / n  ## pvalue
  pval <- round(pval, digits = 3)
  ggplot(model, aes(x = model[, 3])) +
    xlab("Contagion Signal") +
    ylab("Density") +
    ggtitle(title) +
    theme_light() +
    geom_density(fill = "lightgrey", color = "black") +
    geom_vline(aes(xintercept = mean), colour = "black") +
    annotate("text", x = (mean.xcoord), y = mean.ycoord,
             label = paste("Mean:", round(mean, 4)), color = "gray20") +
    geom_vline(aes(xintercept = 0),color = "gray20", linetype = "dashed") +
    annotate("text", x = (pval.xcoord), y = pval.ycoord,
             label = paste("p-value:", pval), color = "gray20")
}
