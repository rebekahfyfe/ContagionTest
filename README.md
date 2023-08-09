# ContagionTest
## About
R package for the network contagion test described in Shalizi and Thomas (2011). Find the most recent draft of the paper in which we present the first applications of this software [here](https://pennstateoffice365-my.sharepoint.com/:b:/g/personal/bbd5087_psu_edu/EQ0lxeGC9qVKv4G12p4Y0jUBo72PLf748lHDoH-dTd7dUg?e=gh5cHN).

## Installation
The package can be installed from GitHub by using devtools.
```{r}
install.packages("devtools")
devtools::install_github("rebekahfyfe/ContagionTest")
```

## Example
```{r}
# load package
library(ContagionTest)

# load polity data included in package
data(polityData)

# reformat data so each column is a different node (country) and each row is a different time period (year)
polityData <- STFormat(polityData)

# run split-halves test
results <- lag_pc_test(df = polityData, iterations = 100, cores = 2, difference = T, threshold = 0.1, lagWin = 1, missingData = F)

# create a dataframe with results
results <- as.data.frame(results)
names(results) <- c("intercept","t-1coef","counterpart")

# calculate mean (contagion signal)
mean <- mean(results$counterpart) ## input this in the function below
mean <- round(mean, digits = 3)
# calculate significance of the signal (proportion of means less than 0)
pval <- sum(results$counterpart < 0) / 100  ## pvalue
pval <- round(pval, digits = 3)

# graph results
density_graph(results, 100, mean, 1, mean, 0)
```
