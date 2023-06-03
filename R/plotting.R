density_graph <- function(model, mean, pval, mean.xcoord, mean.ycoord, pval.xcoord, pval.ycoord){
  ggplot(model, aes(x = counterpart)) +
    #ggtitle("(45 Days)") +
    xlab("Contagion Signal") +
    ylab("Density") +
    theme_light() +
    geom_density(fill = "lightgrey", color = "black") +
    geom_vline(aes(xintercept = mean(counterpart)), colour="black") +
    annotate("text", x = (mean.xcoord), y = mean.ycoord,
             label = paste("Mean:", round(mean, 4)), color = "gray20") +
    geom_vline(aes(xintercept = 0),color = "gray20", linetype = "dashed") +
    annotate("text", x = (pval.xcoord), y = pval.ycoord,
             label = paste("p-value:", pval), color = "gray20")

}
