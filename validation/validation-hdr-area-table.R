# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
res_010 = readRDS(file = "validation/conditional-hdr-area-010.Rds")
res_025 = readRDS(file = "validation/conditional-hdr-area-025.Rds")
res_050 = readRDS(file = "validation/conditional-hdr-area-050.Rds")
res_075 = readRDS(file = "validation/conditional-hdr-area-075.Rds")
res_090 = readRDS(file = "validation/conditional-hdr-area-090.Rds")

colnames(res_010) = c("seam","pitcher","batter")
colnames(res_025) = c("seam","pitcher","batter")
colnames(res_050) = c("seam","pitcher","batter")
colnames(res_075) = c("seam","pitcher","batter")
colnames(res_090) = c("seam","pitcher","batter")

res_mean = rbind(
  colMeans(res_010),
  colMeans(res_025),
  colMeans(res_050),
  colMeans(res_075),
  colMeans(res_090)
)

colnames(res_mean) = c("seam", "pitcher", "batter")
rownames(res_mean) = c("0.10", "0.25", "0.50", "0.75", "0.90")

# preview table
res_mean

# output with latex formatting
knitr::kable(t(res_mean)[, 3:5], format = "latex")

n = nrow(res_090)
res_se = rbind(
  apply(res_010, 2, function(x) sd(x) / n),
  apply(res_025, 2, function(x) sd(x) / n),
  apply(res_050, 2, function(x) sd(x) / n),
  apply(res_075, 2, function(x) sd(x) / n),
  apply(res_090, 2, function(x) sd(x) / n)
)

colnames(res_se) = c("seam", "pitcher", "batter")
rownames(res_se) = c("0.10", "0.25", "0.50", "0.75", "0.90")

# preview table
res_se

# output with latex formatting
knitr::kable(res_se, format = "latex")

res_median = rbind(
  apply(res_010, 2, median),
  apply(res_025, 2, median),
  apply(res_050, 2, median),
  apply(res_075, 2, median),
  apply(res_090, 2, median)
)

colnames(res_median) = c("seam", "pitcher", "batter")
rownames(res_median) = c("0.10", "0.25", "0.50", "0.75", "0.90")

# preview table
res_median

# output with latex formatting
knitr::kable(res_median, format = "latex")
