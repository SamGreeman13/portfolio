library(bootstrap)
library(boot)

data.X <- c(4.23, 1.30, 0.99, 2.41, 1.87, 3.28, 0.67, 3.12, 0.71, 0.82,
            0.67, 0.88, 2.14, 0.29, 2.41, 3.09, 1.02, 0.57, 0.46, 1.381, 0.122)
mean(data.X)

mean_j <- jackknife(data.X, mean)
mean_j

# Bias-corrected jackknife estimate
adj_mean_j = mean(data.X) - mean_j$jack.bias
adj_mean_j
# Sample standard deviation
sd(data.X)
# Jackknife the standard deviation
sd_j <- jackknife(data.X, sd)
sd_j
# Bias-corrected jackknife estimate
adj_sd_j = sd(data.X) - sd_j$jack.bias
adj_sd_j

# Sample variance
var(data.X)
# Jackknife the variance
var_j <- jackknife(data.X,var)
var_j
# Bias-corrected jackknife estimate
adj_var_j = var(data.X) - var_j$jack.bias
adj_var_j

#bootstrap example
data.Y <- read.table("https://stats.idre.ucla.edu/stat/data/hsb2.csv", sep=",", header=T)

#function to obtain correlation
cor_fun <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$write, d2$math))
}
#results
boot_results <- boot(data.Y, cor_fun, R = 500)
boot_results

