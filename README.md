# ContagionTest
## About
R package for the network contagion test described in Shalizi and Thomas (2011).

## Installation
The package can be installed from GitHub by using devtools.
```{r}
install.packages("devtools")
devtools::install_github("rebekahfyfe/ContagionTest")
```

## Example
```{r}
# load polity data included in package
data(polityData)

# reformat data so each column is a different node (country) and each row is a different time period (year)
polityData <- STFormat(polityData)

# run split-halves test
results <- lag_pc_test(data = polityData, iterations = 100, cores = 2, difference = T, threshold = 0.1, lagWin = 1, missingData = F)

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
density_graph(results, mean, pval, mean, 1, mean, 0)
```
